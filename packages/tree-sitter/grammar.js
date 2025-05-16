/**
 * @file Bonzai grammar for tree-sitter
 * @author Thomas Vergne <contact@thomas-vergne.fr>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "bonzai",

  conflicts: ($) => [
    [$.expr_record, $.expr_variable],
    [$.expr_mut, $.expr_variable],
    [$._labelled, $.expr_variable],
    [$._statements],
  ],

  rules: {
    // TODO: add the actual grammar rules
    source_file: ($) => repeat1($.toplevel),

    toplevel: ($) =>
      choice(
        $.top_public,
        $.top_datatype,
        $.top_direct_datatype,
        $.top_require,
        $.top_extern,
        $.statement,
      ),

    top_public: ($) => seq("pub", $.toplevel),

    top_datatype: ($) =>
      seq(
        "type",
        field("name", $._header),
        "{",
        commaSep($._data_constructor),
        "}",
      ),

    top_direct_datatype: ($) =>
      seq(
        "type",
        field("name", $._header),
        "(",
        commaSep($._explicit_annotation),
        ")",
      ),

    top_require: ($) =>
      seq(
        "require",
        field("name", $._string),
        optional(seq(":", commaSep1($._identifier))),
      ),

    top_extern: ($) =>
      seq(
        "extern",
        "fn",
        field("name", $._header),
        "(",
        commaSep($._explicit_annotation),
        ")",
      ),

    statement: ($) =>
      choice(
        $.stmt_while,
        $.stmt_for_in,
        $.stmt_function,
        $.stmt_update,
        $.expression,
      ),

    stmt_while: ($) =>
      seq(
        "while",
        field("condition", $.expression),
        "{",
        repeat($._statements),
        "}",
      ),

    stmt_for_in: ($) =>
      seq(
        "for",
        field("name", $._identifier),
        "in",
        field("collection", $.expression),
        "{",
        repeat($._statements),
        "}",
      ),

    stmt_function: ($) =>
      seq(
        "fn",
        field("name", $._header),
        "(",
        commaSep($._opt_annotation),
        ")",
        field("return_type", optional(seq(":", $.type))),
        "=>",
        field("body", $.expression),
      ),

    stmt_update: ($) =>
      seq(field("name", $._identifier), "=", field("value", $.expression)),

    type: ($) => $._identifier,

    _statements: ($) =>
      seq($.statement, optional(seq(optional(";"), repeat($.statement)))),

    expression: ($) =>
      choice(
        field("binary_operation", $.expr_binary),
        field("lambda", $.expr_lambda),
        field("let", $.expr_let),
        field("spawn", $.expr_spawn),
        field("mut", $.expr_mut),
        field("mut_expr", $.expr_mut_expr),
        field("match", $.expr_match),
        field("ternary", $.expr_ternary),
        field("literal", $.expr_literal),
        field("record", $.expr_record),
        field("block", $.expr_block),
        field("list", $.expr_list),
        field("tuple", $.expr_tuple),
        field("variable", $.expr_variable),
      ),

    expr_binary: ($) =>
      choice(
        prec.right(
          4,
          seq(
            $.expression,
            choice(
              "+=",
              "-=",
              "*=",
              "/=",
              "%=",
              "&=",
              "|=",
              "^=",
              "<<=",
              ">>=",
            ),
            $.expression,
          ),
        ),

        prec(
          15,
          seq(
            field("object", $.expression),
            "->",
            field("field", $._identifier),
          ),
        ),

        prec(
          15,
          seq(
            field("callee", $.expression),
            "(",
            commaSep(choice($.expression, $._labelled)),
            ")",
          ),
        ),

        prec(
          15,
          seq(
            field("array", $.expression),
            "[",
            field("index", $.expression),
            "]",
          ),
        ),

        prec.left(
          13,
          seq(
            field("left", $.expression),
            field("op", choice("*", "/")),
            field("right", $.expression),
          ),
        ),

        prec.left(
          12,
          seq(
            field("left", $.expression),
            field("op", choice("+", "-")),
            field("right", $.expression),
          ),
        ),
        prec.left(
          11,
          seq(
            field("left", $.expression),
            field("op", choice("<<", ">>")),
            field("right", $.expression),
          ),
        ),
        prec.left(
          10,
          seq(
            field("left", $.expression),
            field("op", choice("<", ">", "<=", ">=")),
            field("right", $.expression),
          ),
        ),
        prec.left(
          9,
          seq(
            field("left", $.expression),
            field("op", choice("==", "!=")),
            field("right", $.expression),
          ),
        ),
        prec.left(
          8,
          seq(
            field("left", $.expression),
            field("op", choice("&&", "||")),
            field("right", $.expression),
          ),
        ),
        prec.left(
          7,
          seq(
            field("left", $.expression),
            field("op", choice("&", "|", "^")),
            field("right", $.expression),
          ),
        ),
        prec(14, seq("!", $.expression)),

        prec.left(
          6,
          seq(
            field("left", $.expression),
            field("op", $._operator),
            field("right", $.expression),
          ),
        ),

        prec.left(
          5,
          seq(
            field("left", $.expression),
            ":",
            field("callee", $._identifier),
            ":",
            field("right", $.expression),
          ),
        ),
      ),

    _labelled: ($) =>
      seq(field("name", $._identifier), ":", field("value", $.expression)),

    expr_lambda: ($) =>
      seq(
        "fn",
        "(",
        commaSep1($._opt_annotation),
        ")",
        "=>",
        field("body", $.expression),
      ),

    expr_let: ($) =>
      prec.left(
        0,
        seq(
          "let",
          field("name", $._identifier),
          optional(seq(":", field("type", $.type))),
          "=",
          field("value", $.expression),
          field("body", optional(seq("in", $.expression))),
        ),
      ),

    expr_spawn: ($) => seq("spawn", $.expression),

    expr_mut: ($) =>
      prec.left(
        0,
        seq(
          "mut",
          field("name", $._identifier),
          optional(seq(":", field("type", $.type))),
          "=",
          field("value", $.expression),
          field("body", optional(seq("in", $.expression))),
        ),
      ),

    expr_mut_expr: ($) => seq("mut", $.expression),

    expr_match: ($) =>
      seq(
        "match",
        field("value", $.expression),
        "{",
        repeat(
          seq(
            field("pattern", $._identifier),
            "=>",
            field("body", $.expression),
          ),
        ),
        "}",
      ),

    expr_ternary: ($) =>
      seq(
        "if",
        field("condition", $.expression),
        "then",
        field("then", $.expression),
        "else",
        field("else", $.expression),
      ),

    expr_literal: ($) => choice($._string, /\d+/, /\d+\.\d+/, "true", "false"),

    expr_record: ($) =>
      seq(
        "{",
        commaSep(
          seq(field("name", $._identifier), ":", field("value", $.expression)),
        ),
        "}",
      ),

    expr_block: ($) => seq("{", $._statements, "}"),

    expr_list: ($) => seq("[", commaSep($.expression), "]"),

    expr_tuple: ($) => seq("(", commaSep($.expression), ")"),

    expr_variable: ($) => field("variable", $._identifier),

    _data_constructor: ($) =>
      seq(
        field("name", $._identifier),
        optional(seq("(", commaSep($._explicit_annotation), ")")),
      ),

    _opt_annotation: ($) =>
      seq(
        field("name", $._identifier),
        field("type", optional(seq(":", $.type))),
      ),

    _explicit_annotation: ($) => seq($._identifier, ":", field("type", $.type)),

    _header: ($) =>
      seq(
        field("name", $._identifier),
        field("generics", optional($._generics)),
      ),

    _string: ($) => /\"([^"\\]|\\.)*\"/,
    _identifier: ($) => /[a-zA-Z\$_][a-zA-Z0-9\$_]*/,
    _generics: ($) => seq("<", commaSep1($._identifier), ">"),

    _operator: ($) => {
      const validOperators = [
        "!",
        "#",
        "$",
        "%",
        "&",
        "*",
        "+",
        ".",
        "/",
        "<",
        "=",
        ">",
        "?",
        "@",
        "^",
        "|",
        "-",
        "~",
      ];

      const operatorChars = validOperators.map((x) => `\\\\${x}`).join("");

      const operatorRegex = `[${operatorChars}]+`;

      return new RegExp(operatorRegex);
    },
  },
});

/**
 * Creates a rule to match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @returns {SeqRule}
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

/**
 * Creates a rule to optionally match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @returns {ChoiceRule}
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}
