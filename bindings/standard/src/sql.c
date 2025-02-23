#include <sql.h>
#include <libpq-fe.h>
#include <error.h>
#include <values.h>

Value psql_connect(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "connect", argc, 1);
  ASSERT_TYPE(module, "connect", args[0], TYPE_STRING);

  char* conninfo = GET_STRING(args[0]);

  PGconn *conn = PQconnectdb(conninfo);
  if (PQstatus(conn) == CONNECTION_BAD) {
    char* error = strdup(PQerrorMessage(conn));
    PQfinish(conn);
    return throwable_error(module, error);
  }

  HeapValue* conn_value = allocate(module, TYPE_API);
  conn_value->as_any = conn;


  return throwable_ok(module, MAKE_PTR(conn_value));
}

Value psql_query(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "query", argc, 2);
  ASSERT_TYPE(module, "query", args[0], TYPE_API);
  ASSERT_TYPE(module, "query", args[1], TYPE_STRING);

  HeapValue* conn_value = GET_PTR(args[0]);
  PGconn* conn = conn_value->as_any;
  char* query = GET_STRING(args[1]);

  PGresult* res = PQexec(conn, query);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    char* error = strdup(PQresultErrorMessage(res));
    PQclear(res);
    return throwable_error(module, error);
  }

  // Get number of rows and columns
  int rows = PQntuples(res);
  int cols = PQnfields(res);

  // Create a list of lists to store the result
  Value* res_value = malloc(rows * sizeof(Value));
  for (int i = 0; i < rows; i++) {
    Value* row = malloc(cols * sizeof(Value));
    for (int j = 0; j < cols; j++) {
      char* value = PQgetvalue(res, i, j);
      row[j] = MAKE_STRING_NON_GC(module, value);
    }
    res_value[i] = MAKE_LIST(module, row, cols);
  }

  PQclear(res);

  return throwable_ok(module, MAKE_LIST(module, res_value, rows));
}

Value psql_close_connection(Module* module, Value* args, int argc) {
  ASSERT_ARGC(module, "close_connection", argc, 1);
  ASSERT_TYPE(module, "close_connection", args[0], TYPE_API);

  HeapValue* conn_value = GET_PTR(args[0]);
  PGconn* conn = conn_value->as_any;

  PQfinish(conn);

  return MAKE_SPECIAL();
}