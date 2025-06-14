#include <module.h>
#include <pthread.h>
#include <value.h>

Value wait_thread(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "wait_thread", argc, 1);
  ASSERT_TYPE(mod, "wait_thread", args[0], TYPE_THREAD);

  HeapValue* thread_value = GET_PTR(args[0]);
  pthread_t thread = thread_value->as_event.thread;
  atomic_store(&mod->stack->is_stopped, true);
  atomic_fetch_add(&mod->gc->thread_stopped, 1);
  Value* thread_returned;
  int join_result = pthread_join(thread, (void**)&thread_returned);
  atomic_store(&mod->stack->is_stopped, false);
  atomic_fetch_sub(&mod->gc->thread_stopped, 1);

  if (join_result != 0 || thread_returned == NULL) {
    THROW(mod, "Thread join failed or returned NULL");
  }

  Value object = *thread_returned;

  free(thread_returned);

  return object;
}

Value lock(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "lock_mutex", argc, 1);

  if (IS_EMPTY_LIST(args[0]) || IS_EMPTY_RECORD(args[0])) {
    return MAKE_INTEGER(1);
  } 

  if (!IS_PTR(args[0])) {
    THROW(mod, "Argument to lock_mutex must be a pointer");
  }

  HeapValue* mutex = GET_PTR(args[0]);

  if (pthread_mutex_trylock(&mutex->mutex) != 0) {
    // If the mutex is already locked, we can either wait or return an error.
    // Here we choose to return an error.
    THROW(mod, "Mutex is already locked");
  }

  return MAKE_INTEGER(1);
}

Value mutex_trylock(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "trylock_mutex", argc, 1);

  if (IS_EMPTY_LIST(args[0]) || IS_EMPTY_RECORD(args[0])) {
    return MAKE_INTEGER(1);
  }

  if (!IS_PTR(args[0])) {
    THROW(mod, "Argument to trylock_mutex must be a pointer");
  }

  HeapValue* mutex = GET_PTR(args[0]);

  if (pthread_mutex_trylock(&mutex->mutex) == 0) {
    return MAKE_INTEGER(1); // Successfully locked
  } else {
    return MAKE_INTEGER(0); // Mutex is already locked
  }
}

Value unlock(Module* mod, Value* args, int argc) {
  ASSERT_ARGC(mod, "lock_mutex", argc, 1);

  if (IS_EMPTY_LIST(args[0]) || IS_EMPTY_RECORD(args[0])) {
    return MAKE_INTEGER(0);
  }

  if (!IS_PTR(args[0])) {
    THROW(mod, "Argument to lock_mutex must be a pointer");
  }

  HeapValue* mutex = GET_PTR(args[0]);

  pthread_mutex_unlock(&mutex->mutex);


  return MAKE_INTEGER(0);
}
