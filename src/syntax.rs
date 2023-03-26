use crate::grammar::{Rules, VisitRules, VisitTerminals, Term};

pub struct Syntax;

impl Rules for Syntax {
    fn walk_rules(&self, f: &mut impl VisitRules) {
        // Basics

        f.def("translation-unit")
            .alt("declaration-seq".opt())
            .alt(("global-module-fragment".opt(), "module-declaration", "declaration-seq".opt(), "private-module-fragment".opt()))
        ;

        // Lexical conventions

        f.def("literal")
            .alt("integer-literal")
            .alt("character-literal")
            .alt("floating-point-literal")
            .alt("string-literal")
            .alt("boolean-literal")
            .alt("pointer-literal")
            .alt("user-defined-literal")
        ;

        f.def("boolean-literal")
            .alt("true")
            .alt("false")
        ;

        f.def("pointer-literal")
            .alt("nullptr")
        ;

        f.def("user-defined-literal")
            .alt("user-defined-integer-literal")
            .alt("user-defined-floating-point-literal")
            .alt("user-defined-string-literal")
            .alt("user-defined-character-literal")
        ;

        // Expressions

        f.def("primary-expression")
            .alt("literal")
            .alt("this")
            .alt(("(", "expression", ")"))
            .alt("id-expression")
            .alt("lambda-expression")
            .alt("fold-expression")
            .alt("requires-expression")
        ;

        f.def("id-expression")
            .alt("unqualified-id")
            .alt("qualified-id")
        ;

        f.def("unqualified-id")
            .alt("identifier")
            .alt("operator-function-id")
            .alt("conversion-function-id")
            .alt("literal-operator-id")
            .alt(("~", "type-name"))
            .alt(("~", "decltype-specifier"))
            .alt("template-id")
        ;

        f.def("qualified-id")
            .alt(("nested-name-specifier", "template".opt(), "unqualified-id"));

        f.def("nested-name-specifier")
            .alt("::")
            .alt(("type-name", "::"))
            .alt(("namespace-name", "::"))
            .alt(("decltype-specifier", "::"))
            .alt(("nested-name-specifier", "identifier", "::"))
            .alt(("nested-name-specifier", "template".opt(), "simple-template-id", "::"))
        ;

        f.def("lambda-expression")
            .alt(("lambda-introducer", "attribute-specifier-seq".opt(), "lambda-declarator", "compound-statement"))
            .alt(("lambda-introducer", "<", "template-parameter-list", ">", "requires-clause".opt(), "attribute-specifier-seq".opt(), "lambda-declarator", "compound-statement"))
        ;

        f.def("lambda-introducer")
            .alt(("[", "lambda-capture".opt(), "]"));

        f.def("lambda-declarator")
            .alt(("lambda-specifier-seq", "noexcept-specifier".opt(), "attribute-specifier-seq".opt(), "trailing-return-type".opt()))
            .alt(("noexcept-specifier", "attribute-specifier-seq".opt(), "trailing-return-type".opt()))
            .alt("trailing-return-type")
            .alt(("(", "parameter-declaration-clause", ")", "lambda-specifier-seq".opt(), "noexcept-specifier".opt(), "attribute-specifier-seq".opt(), "trailing-return-type".opt(), "requires-clause".opt()))
        ;

        f.def("lambda-specifier")
            .alt("consteval")
            .alt("constexpr")
            .alt("mutable")
            .alt("static")
        ;

        f.def("lambda-specifier-seq")
            .alt("lambda-specifier")
            .alt(("lambda-specifier", "lambda-specifier-seq"))
        ;

        f.def("lambda-capture")
            .alt("capture-default")
            .alt("capture-list")
            .alt(("capture-default", ",", "capture-list"))
        ;

        f.def("capture-default")
            .alt("&")
            .alt("=")
        ;

        f.def("capture-list")
            .alt("capture")
            .alt(("capture-list", ",", "capture"))
        ;

        f.def("capture")
            .alt("simple-capture")
            .alt("init-capture")
        ;

        f.def("simple-capture")
            .alt(("identifier", "...".opt()))
            .alt(("&", "identifier", "...".opt()))
            .alt("this")
            .alt(("*", "this"))
        ;

        f.def("init-capture")
            .alt(("...".opt(), "identifier", "initializer"))
            .alt(("&", "...".opt(), "identifier", "initializer"))
        ;

        f.def("fold-expression")
            .alt(("(", "cast-expression", "fold-operator", "...", ")"))
            .alt(("(", "...", "fold-operator", "cast-expression", ")"))
            .alt(("(", "cast-expression", "fold-operator", "...", "fold-operator", "cast-expression", ")"))
        ;

        f.def("fold-operator")
            .alt("+").alt("-").alt("*").alt("/").alt("%").alt("^").alt("&").alt("|").alt("<<").alt(">>")
            .alt("+=").alt("-=").alt("*=").alt("/=").alt("%=").alt("^=").alt("&=").alt("|=").alt("<<=").alt(">>=").alt("=")
            .alt("==").alt("!=").alt("<").alt(">").alt("<=").alt(">=").alt("&&").alt("||").alt(",").alt(".*").alt("->*")
        ;

        f.def("requires-expression")
            .alt(("requires", "requirement-parameter-list".opt(), "requirement-body"));

        f.def("requirement-parameter-list")
            .alt(("(", "parameter-declaration-clause", ")"));

        f.def("requirement-body")
            .alt(("{", "requirement-seq", "}"));

        f.def("requirement-seq")
            .alt("requirement")
            .alt(("requirement", "requirement-seq"))
        ;

        f.def("requirement")
            .alt("simple-requirement")
            .alt("type-requirement")
            .alt("compound-requirement")
            .alt("nested-requirement")
        ;

        f.def("simple-requirement")
            .alt(("expression", ";"));

        f.def("type-requirement")
            .alt(("typename", "nested-name-specifier".opt(), "type-name", ";"));

        f.def("compound-requirement")
            .alt(("{", "expression", "}", "noexcept".opt(), "return-type-requirement".opt(), ";"));

        f.def("return-type-requirement")
            .alt(("->", "type-constraint"));

        f.def("nested-requirement")
            .alt(("requires", "constraint-expression", ";"));

        f.def("postfix-expression")
            .alt("primary-expression")
            .alt(("postfix-expression", "[", "expression-list".opt(), "]"))
            .alt(("postfix-expression", "(", "expression-list".opt(), ")"))
            .alt(("simple-type-specifier", "(", "expression-list".opt(), ")"))
            .alt(("typename-specifier", "(", "expression-list".opt(), ")"))
            .alt(("simple-type-specifier", "braced-init-list"))
            .alt(("typename-specifier", "braced-init-list"))
            .alt(("postfix-expression", ".", "template".opt(), "id-expression"))
            .alt(("postfix-expression", "->", "template".opt(), "id-expression"))
            .alt(("postfix-expression", "++"))
            .alt(("postfix-expression", "--"))
            .alt(("dynamic_cast", "<", "type-id", ">", "(", "expression", ")"))
            .alt(("static_cast", "<", "type-id", ">", "(", "expression", ")"))
            .alt(("reinterpret_cast", "<", "type-id", ">", "(", "expression", ")"))
            .alt(("const_cast", "<", "type-id", ">", "(", "expression", ")"))
            .alt(("typeid", "(", "expression", ")"))
            .alt(("typeid", "(", "type-id", ")"))
        ;

        f.def("expression-list")
            .alt("initializer-list");

        f.def("unary-expression")
            .alt("postfix-expression")
            .alt(("unary-operator", "cast-expression"))
            .alt(("++", "cast-expression"))
            .alt(("--", "cast-expression"))
            .alt("await-expression")
            .alt(("sizeof", "unary-expression"))
            .alt(("sizeof", "(", "type-id", ")"))
            .alt(("sizeof", "...", "(", "identifier", ")"))
            .alt(("alignof", "(", "type-id", ")"))
            .alt("noexcept-expression")
            .alt("new-expression")
            .alt("delete-expression")
        ;

        f.def("unary-operator")
            .alt("*").alt("&").alt("+").alt("-").alt("!").alt("~");

        f.def("await-expression")
            .alt(("co_await", "cast-expression"));

        f.def("noexcept-expression")
            .alt(("noexcept", "(", "expression", ")"));

        f.def("new-expression")
            .alt(("::".opt(), "new", "new-placement".opt(), "new-type-id", "new-initializer".opt()))
            .alt(("::".opt(), "new", "new-placement".opt(), "(", "type-id", ")", "new-initializer".opt()))
        ;

        f.def("new-placement")
            .alt(("(", "expression-list", ")"));

        f.def("new-type-id")
            .alt(("type-specifier-seq", "new-declarator".opt()));

        f.def("new-declarator")
            .alt(("ptr-operator", "new-declarator".opt()))
            .alt("noptr-new-declarator")
        ;

        f.def("noptr-new-declarator")
            .alt(("[", "expression".opt(), "]", "attribute-specifier-seq".opt()))
            .alt(("noptr-new-declarator", "[", "constant-expression", "]", "attribute-specifier-seq".opt()))
        ;

        f.def("new-initializer")
            .alt(("(", "expression-list".opt(), ")"))
            .alt("braced-init-list")
        ;

        f.def("delete-expression")
            .alt(("::".opt(), "delete", "cast-expression"))
            .alt(("::".opt(), "delete", "[", "]", "cast-expression"))
        ;

        f.def("cast-expression")
            .alt("unary-expression")
            .alt(("(", "type-id", ")", "cast-expression"))
        ;

        f.def("pm-expression")
            .alt("cast-expression")
            .alt(("pm-expression", ".*", "cast-expression"))
            .alt(("pm-expression", "->*", "cast-expression"))
        ;

        f.def("multiplicative-expression")
            .alt("pm-expression")
            .alt(("multiplicative-expression", "*", "pm-expression"))
            .alt(("multiplicative-expression", "/", "pm-expression"))
            .alt(("multiplicative-expression", "%", "pm-expression"))
        ;

        f.def("additive-expression")
            .alt("multiplicative-expression")
            .alt(("additive-expression", "+", "multiplicative-expression"))
            .alt(("additive-expression", "-", "multiplicative-expression"))
        ;

        f.def("shift-expression")
            .alt("additive-expression")
            .alt(("shift-expression", "<<", "additive-expression"))
            .alt(("shift-expression", ">>", "additive-expression"))
        ;

        f.def("compare-expression")
            .alt("shift-expression")
            .alt(("compare-expression", "<=>", "shift-expression"))
        ;

        f.def("relational-expression")
            .alt("compare-expression")
            .alt(("relational-expression", "<<", "compare-expression"))
            .alt(("relational-expression", ">>", "compare-expression"))
        ;

        f.def("equality-expression")
            .alt("relational-expression")
            .alt(("equality-expression", "==", "relational-expression"))
            .alt(("equality-expression", "!=", "relational-expression"))
        ;

        f.def("and-expression")
            .alt("equality-expression")
            .alt(("and-expression", "&", "equality-expression"))
        ;

        f.def("exclusive-or-expression")
            .alt("and-expression")
            .alt(("exclusive-or-expression", "^", "and-expression"))
        ;

        f.def("inclusive-or-expression")
            .alt("exclusive-or-expression")
            .alt(("inclusive-or-expression", "|", "exclusive-or-expression"))
        ;

        f.def("logical-and-expression")
            .alt("inclusive-or-expression")
            .alt(("logical-and-expression", "&&", "inclusive-or-expression"))
        ;

        f.def("logical-or-expression")
            .alt("logical-and-expression")
            .alt(("logical-or-expression", "||", "logical-and-expression"))
        ;

        f.def("conditional-expression")
            .alt("logical-or-expression")
            .alt(("logical-or-expression", "?", "expression", ":", "assignment-expression"))
        ;

        f.def("yield-expression")
            .alt(("co_yield", "assignment-expression"))
            .alt(("co_yield", "braced-init-list"))
        ;

        f.def("throw-expression")
            .alt(("throw", "assignment-expression".opt()));

        f.def("assignment-expression")
            .alt("conditional-expression")
            .alt("yield-expression")
            .alt("throw-expression")
            .alt(("logical-or-expression", "assignment-operator", "initializer-clause"))
        ;

        f.def("assignment-operator")
            .alt("=").alt("*=").alt("/=").alt("%=").alt("+=").alt("-=").alt(">>=").alt("<<=").alt("&=").alt("^=").alt("|=");

        f.def("expression")
            .alt("assignment-expression")
            .alt(("expression", ",", "assignment-expression"))
        ;

        f.def("constant-expression")
            .alt("conditional-expression");

        // Statements

        f.def("statement")
            .alt("labeled-statement")
            .alt(("attribute-specifier-seq".opt(), "expression-statement"))
            .alt(("attribute-specifier-seq".opt(), "compound-statement"))
            .alt(("attribute-specifier-seq".opt(), "selection-statement"))
            .alt(("attribute-specifier-seq".opt(), "iteration-statement"))
            .alt(("attribute-specifier-seq".opt(), "jump-statement"))
            .alt("declaration-statement")
            .alt(("attribute-specifier-seq".opt(), "try-block"))
        ;

        f.def("init-statement")
            .alt("expression-statement")
            .alt("simple-declaration")
            .alt("alias-declaration")
        ;

        f.def("condition")
            .alt("expression")
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq", "declarator", "brace-or-equal-initializer"))
        ;

        f.def("label")
            .alt(("attribute-specifier-seq".opt(), "identifier", ":"))
            .alt(("attribute-specifier-seq".opt(), "case", "constant-expression", ":"))
            .alt(("attribute-specifier-seq".opt(), "default", ":"))
        ;

        f.def("labeled-statement")
            .alt(("label", "statement"));

        f.def("expression-statement")
            .alt(("expression".opt(), ";"));

        f.def("compound-statement")
            .alt(("{", "statement-seq".opt(), "label-seq".opt(), "}"));

        f.def("statement-seq")
            .alt("statement")
            .alt(("statement-seq", "statement"))
        ;

        f.def("label-seq")
            .alt("label")
            .alt(("label-seq", "label"))
        ;

        f.def("selection-statement")
            .alt(("if", "constexpr".opt(), "(", "init-statement".opt(), "condition", ")", "statement"))
            .alt(("if", "constexpr".opt(), "(", "init-statement".opt(), "condition", ")", "statement", "else", "statement"))
            .alt(("if", "!".opt(), "consteval", "compound-statement"))
            .alt(("if", "!".opt(), "consteval", "compound-statement", "else", "statement"))
            .alt(("switch", "(", "init-statement".opt(), "condition", ")", "statement"))
        ;

        f.def("iteration-statement")
            .alt(("while", "(", "condition", ")", "statement"))
            .alt(("do", "statement", "while", "(", "expression", ")", ";"))
            .alt(("for", "(", "init-statement", "condition".opt(), ";", "expression".opt(), ")", "statement"))
            .alt(("for", "(", "init-statement".opt(), "for-range-declaration", ":", "for-range-initializer", ")", "statement"))
        ;

        f.def("for-range-declaration")
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq", "declarator"))
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq", "ref-qualifier".opt(), "[", "identifier-list", "]"))
        ;

        f.def("for-range-initializer")
            .alt("expr-or-braced-init-list");

        f.def("jump-statement")
            .alt(("break", ";"))
            .alt(("continue", ";"))
            .alt(("return", "expr-or-braced-init-list".opt(), ";"))
            .alt("coroutine-return-statement")
            .alt(("goto", "identifier", ";"))
        ;

        f.def("coroutine-return-statement")
            .alt(("co_return", "expr-or-braced-init-list".opt(), ";"));

        f.def("declaration-statement")
            .alt("block-declaration");

        // Declarations

        f.def("declaration-seq")
            .alt("declaration")
            .alt(("declaration-seq", "declaration"))
        ;

        f.def("declaration")
            .alt("name-declaration")
            .alt("special-declaration")
        ;

        f.def("name-declaration")
            .alt("block-declaration")
            .alt("nodeclspec-function-declaration")
            .alt("function-definition")
            .alt("template-declaration")
            .alt("deduction-guide")
            .alt("linkage-specification")
            .alt("namespace-definition")
            .alt("empty-declaration")
            .alt("attribute-declaration")
            .alt("module-import-declaration")
        ;

        f.def("special-declaration")
            .alt("explicit-instantiation")
            .alt("explicit-specialization")
            .alt("export-declaration")
        ;

        f.def("block-declaration")
            .alt("simple-declaration")
            .alt("asm-declaration")
            .alt("namespace-alias-definition")
            .alt("using-declaration")
            .alt("using-enum-declaration")
            .alt("using-directive")
            .alt("static_assert-declaration")
            .alt("alias-declaration")
            .alt("opaque-enum-declaration")
        ;

        f.def("nodeclspec-function-declaration")
            .alt(("attribute-specifier-seq".opt(), "declarator", ";"));

        f.def("alias-declaration")
            .alt(("using", "identifier", "attribute-specifier-seq".opt(), "=", "defining-type-id", ";"));

        f.def("simple-declaration")
            .alt(("decl-specifier-seq", "init-declarator-list".opt(), ";"))
            .alt(("attribute-specifier-seq", "decl-specifier-seq", "init-declarator-list", ";"))
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq", "ref-qualifier".opt(), "[", "identifier-list", "]", "initializer", ";"))
        ;

        f.def("static_assert-declaration")
            .alt(("static_assert", "(", "constant-expression", ")", ";"))
            .alt(("static_assert", "(", "constant-expression", ",", "string-literal", ")", ";"))
        ;

        f.def("empty-declaration")
            .alt(";");

        f.def("attribute-declaration")
            .alt(("attribute-specifier-seq", ";"));

        f.def("decl-specifier")
            .alt("storage-class-specifier")
            .alt("defining-type-specifier")
            .alt("function-specifier")
            .alt("friend")
            .alt("typedef")
            .alt("constexpr")
            .alt("consteval")
            .alt("constinit")
            .alt("inline")
        ;

        f.def("decl-specifier-seq")
            .alt(("decl-specifier", "attribute-specifier-seq".opt()))
            .alt(("decl-specifier", "decl-specifier-seq"))
        ;

        f.def("storage-class-specifier")
            .alt("static")
            .alt("thread_local")
            .alt("extern")
            .alt("mutable")
        ;

        f.def("function-specifier")
            .alt("virtual")
            .alt("explicit-specifier")
        ;

        f.def("explicit-specifier")
            .alt(("explicit", "(", "constant-expression", ")"))
            .alt("explicit")
        ;

        f.def("typedef-name")
            .alt("identifier")
            .alt("simple-template-id")
        ;

        f.def("type-specifier")
            .alt("simple-type-specifier")
            .alt("elaborated-type-specifier")
            .alt("typename-specifier")
            .alt("cv-qualifier")
        ;

        f.def("type-specifier-seq")
            .alt(("type-specifier", "attribute-specifier-seq".opt()))
            .alt(("type-specifier", "type-specifier-seq"))
        ;

        f.def("defining-type-specifier")
            .alt("type-specifier")
            .alt("class-specifier")
            .alt("enum-specifier")
        ;

        f.def("defining-type-specifier-seq")
            .alt(("defining-type-specifier", "attribute-specifier-seq".opt()))
            .alt(("defining-type-specifier", "defining-type-specifier-seq"))
        ;

        f.def("simple-type-specifier")
            .alt(("nested-name-specifier".opt(), "type-name"))
            .alt(("nested-name-specifier", "template", "simple-template-id"))
            .alt("decltype-specifier")
            .alt("placeholder-type-specifier")
            .alt(("nested-name-specifier".opt(), "template-name"))
            .alt("char")
            .alt("char8_t")
            .alt("char16_t")
            .alt("char32_t")
            .alt("wchar_t")
            .alt("bool")
            .alt("short")
            .alt("int")
            .alt("long")
            .alt("signed")
            .alt("unsigned")
            .alt("float")
            .alt("double")
            .alt("void")
        ;

        f.def("type-name")
            .alt("class-name")
            .alt("enum-name")
            .alt("typedef-name")
        ;

        f.def("elaborated-type-specifier")
            .alt(("class-key", "attribute-specifier-seq".opt(), "nested-name-specifier".opt(), "identifier"))
            .alt(("class-key", "simple-template-id"))
            .alt(("class-key", "nested-name-specifier", "template".opt(), "simple-template-id"))
            .alt(("enum", "nested-name-specifier".opt(), "identifier"))
        ;

        f.def("decltype-specifier")
            .alt(("decltype", "(", "expression", ")"));

        f.def("placeholder-type-specifier")
            .alt(("type-constraint".opt(), "auto"))
            .alt(("type-constraint".opt(), "decltype", "(", "auto", ")"))
        ;

        f.def("init-declarator-list")
            .alt("init-declarator")
            .alt(("init-declarator-list", ",", "init-declarator"))
        ;

        f.def("init-declarator")
            .alt(("declarator", "initializer".opt()))
            .alt(("declarator", "requires-clause"))
        ;

        f.def("declarator")
            .alt("ptr-declarator")
            .alt(("noptr-declarator", "parameters-and-qualifiers", "trailing-return-type"))
        ;

        f.def("ptr-declarator")
            .alt("noptr-declarator")
            .alt(("ptr-operator", "ptr-declarator"))
        ;

        f.def("noptr-declarator")
            .alt(("declarator-id", "attribute-specifier-seq".opt()))
            .alt(("noptr-declarator", "parameters-and-qualifiers"))
            .alt(("noptr-declarator", "[", "constant-expression".opt(), "]", "attribute-specifier-seq".opt()))
            .alt(("(", "ptr-declarator", ")"))
        ;

        f.def("parameters-and-qualifiers")
            .alt(("(", "parameter-declaration-clause", ")", "cv-qualifier-seq".opt(), "ref-qualifier".opt(), "noexcept-specifier".opt(), "attribute-specifier-seq".opt()));

        f.def("trailing-return-type")
            .alt(("->", "type-id"));

        f.def("ptr-operator")
            .alt(("*", "attribute-specifier-seq".opt(), "cv-qualifier-seq".opt()))
            .alt(("&", "attribute-specifier-seq".opt()))
            .alt(("&&", "attribute-specifier-seq".opt()))
            .alt(("nested-name-specifier", "*", "attribute-specifier-seq".opt(), "cv-qualifier-seq".opt()))
        ;

        f.def("cv-qualifier-seq")
            .alt(("cv-qualifier", "cv-qualifier-seq".opt()));

        f.def("cv-qualifier")
            .alt("const")
            .alt("volatile")
        ;

        f.def("ref-qualifier")
            .alt("&")
            .alt("&&")
        ;

        f.def("declarator-id")
            .alt(("...".opt(), "id-expression"));

        f.def("type-id")
            .alt(("type-specifier-seq", "abstract-declarator".opt()));

        f.def("defining-type-id")
            .alt(("defining-type-specifier-seq", "abstract-declarator".opt()));

        f.def("abstract-declarator")
            .alt("ptr-abstract-declarator")
            .alt(("noptr-abstract-declarator", "parameters-and-qualifiers", "trailing-return-type"))
            .alt("abstract-pack-declarator")
        ;

        f.def("ptr-abstract-declarator")
            .alt("noptr-abstract-declarator")
            .alt(("ptr-operator", "ptr-abstract-declarator".opt()))
        ;

        f.def("noptr-abstract-declarator")
            .alt(("noptr-abstract-declarator".opt(), "parameters-and-qualifiers"))
            .alt(("noptr-abstract-declarator".opt(), "[", "constant-expression".opt(), "]", "attribute-specifier-seq".opt()))
            .alt(("(", "ptr-abstract-declarator", ")"))
        ;

        f.def("abstract-pack-declarator")
            .alt("noptr-abstract-pack-declarator")
            .alt(("ptr-operator", "abstract-pack-declarator"))
        ;

        f.def("noptr-abstract-pack-declarator")
            .alt(("noptr-abstract-pack-declarator", "parameters-and-qualifiers"))
            .alt(("noptr-abstract-pack-declarator", "[", "constant-expression".opt(), "]", "attribute-specifier-seq".opt()))
            .alt("...")
        ;

        f.def("parameter-declaration-clause")
            .alt(("parameter-declaration-list".opt(), "...".opt()))
            .alt(("parameter-declaration-list".opt(), ",", "..."))
        ;

        f.def("parameter-declaration-list")
            .alt("parameter-declaration")
            .alt(("parameter-declaration-list", ",", "parameter-declaration"))
        ;

        f.def("parameter-declaration")
            .alt(("attribute-specifier-seq".opt(), "this".opt(), "decl-specifier-seq", "declarator"))
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq", "declarator", "=", "initializer-clause"))
            .alt(("attribute-specifier-seq".opt(), "this".opt(), "decl-specifier-seq", "abstract-declarator".opt()))
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq", "abstract-declarator".opt(), "=", "initializer-clause"))
        ;

        f.def("initializer")
            .alt("brace-or-equal-initializer")
            .alt(("(", "expression-list", ")"))
        ;

        f.def("brace-or-equal-initializer")
            .alt(("=", "initializer-clause"))
            .alt("braced-init-list")
        ;

        f.def("initializer-clause")
            .alt("assignment-expression")
            .alt("braced-init-list")
        ;

        f.def("braced-init-list")
            .alt(("{", "initializer-list", ",".opt(), "}"))
            .alt(("{", "designated-initializer-list", ",".opt(), "}"))
            .alt(("{", "}"))
        ;

        f.def("initializer-list")
            .alt(("initializer-clause", "...".opt()))
            .alt(("initializer-list", ",", "initializer-clause", "...".opt()))
        ;

        f.def("designated-initializer-list")
            .alt("designated-initializer-clause")
            .alt(("designated-initializer-list", ",", "designated-initializer-clause"))
        ;

        f.def("designated-initializer-clause")
            .alt(("designator", "brace-or-equal-initializer"));

        f.def("designator")
            .alt((".", "identifier"));

        f.def("expr-or-braced-init-list")
            .alt("expression")
            .alt("braced-init-list")
        ;

        f.def("function-definition")
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq".opt(), "declarator", "virt-specifier-seq".opt(), "function-body"))
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq".opt(), "declarator", "requires-clause", "function-body"))
        ;

        f.def("function-body")
            .alt(("ctor-initializer".opt(), "compound-statement"))
            .alt("function-try-block")
            .alt(("=", "default", ";"))
            .alt(("=", "delete", ";"))
        ;

        f.def("enum-name")
            .alt("identifier");

        f.def("enum-specifier")
            .alt(("enum-head", "{", "enumerator-list".opt(), "}"))
            .alt(("enum-head", "{", "enumerator-list", ",", "}"))
        ;

        f.def("enum-head")
            .alt(("enum-key", "attribute-specifier-seq".opt(), "enum-head-name".opt(), "enum-base".opt()));

        f.def("enum-head-name")
            .alt(("nested-name-specifier".opt(), "identifier"));

        f.def("opaque-enum-declaration")
            .alt(("enum-key", "attribute-specifier-seq".opt(), "enum-head-name", "enum-base".opt(), ";"));

        f.def("enum-key")
            .alt("enum")
            .alt(("enum", "class"))
            .alt(("enum", "struct"))
        ;

        f.def("enum-base")
            .alt((":", "type-specifier-seq"));

        f.def("enumerator-list")
            .alt("enumerator-definition")
            .alt(("enumerator-list", ",", "enumerator-definition"))
        ;

        f.def("enumerator-definition")
            .alt("enumerator")
            .alt(("enumerator", "=", "constant-expression"))
        ;

        f.def("enumerator")
            .alt(("identifier", "attribute-specifier-seq".opt()));

        f.def("using-enum-declaration")
            .alt(("using", "enum", "using-enum-declarator", ";"));

        f.def("using-enum-declarator")
            .alt(("nested-name-specifier".opt(), "identifier"))
            .alt(("nested-name-specifier".opt(), "simple-template-id"))
        ;

        f.def("namespace-name")
            .alt("identifier")
            .alt("namespace-alias")
        ;

        f.def("namespace-definition")
            .alt("named-namespace-definition")
            .alt("unnamed-namespace-definition")
            .alt("nested-namespace-definition")
        ;

        f.def("named-namespace-definition")
            .alt(("inline".opt(), "namespace", "attribute-specifier-seq".opt(), "identifier", "{", "namespace-body", "}"));

        f.def("unnamed-namespace-definition")
            .alt(("inline".opt(), "namespace", "attribute-specifier-seq".opt(), "{", "namespace-body", "}"));

        f.def("nested-namespace-definition")
            .alt(("namespace", "enclosing-namespace-specifier", "::", "inline".opt(), "identifier", "{", "namespace-body", "}"));

        f.def("enclosing-namespace-specifier")
            .alt("identifier")
            .alt(("enclosing-namespace-specifier", "::", "inline".opt(), "identifier"))
        ;

        f.def("namespace-body")
            .alt("declaration-seq".opt());

        f.def("namespace-alias")
            .alt("identifier");

        f.def("namespace-alias-definition")
            .alt(("namespace", "identifier", "=", "qualified-namespace-specifier", ";"));

        f.def("qualified-namespace-specifier")
            .alt(("nested-name-specifier".opt(), "namespace-name"));

        f.def("using-directive")
            .alt(("attribute-specifier-seq".opt(), "using", "namespace", "nested-name-specifier".opt(), "namespace-name", ";"));

        f.def("using-declaration")
            .alt(("using", "using-declarator-list", ";"));

        f.def("using-declarator-list")
            .alt(("using-declarator", "...".opt()))
            .alt(("using-declarator-list", ",", "using-declarator", "...".opt()))
        ;

        f.def("using-declarator")
            .alt(("typename".opt(), "nested-name-specifier", "unqualified-id"));

        f.def("asm-declaration")
            .alt(("attribute-specifier-seq".opt(), "asm", "(", "string-literal", ")", ";"));

        f.def("linkage-specification")
            .alt(("extern", "string-literal", "{", "declaration-seq".opt(), "}"))
            .alt(("extern", "string-literal", "name-declaration"))
        ;

        f.def("attribute-specifier-seq")
            .alt(("attribute-specifier-seq".opt(), "attribute-specifier"));

        f.def("attribute-specifier")
            .alt(("[", "[", "attribute-using-prefix".opt(), "attribute-list", "]", "]"))
            .alt("alignment-specifier")
        ;

        f.def("alignment-specifier")
            .alt(("alignas", "(", "type-id", "...".opt(), ")"))
            .alt(("alignas", "(", "constant-expression", "...".opt(), ")"))
        ;

        f.def("attribute-using-prefix")
            .alt(("using", "attribute-namespace", ":"));

        f.def("attribute-list")
            .alt("attribute".opt())
            .alt(("attribute-list", ",", "attribute".opt()))
            .alt(("attribute", "..."))
            .alt(("attribute-list", ",", "attribute", "..."))
        ;

        f.def("attribute")
            .alt(("attribute-token", "attribute-argument-clause".opt()));

        f.def("attribute-token")
            .alt("identifier")
            .alt("attribute-scoped-token")
        ;

        f.def("attribute-scoped-token")
            .alt(("attribute-namespace", "::", "identifier"));

        f.def("attribute-namespace")
            .alt("identifier");

        f.def("attribute-argument-clause")
            .alt(("(", "balanced-token-seq".opt(), ")"));

        f.def("balanced-token-seq")
            .alt("balanced-token")
            .alt(("balanced-token-seq", "balanced-token"))
        ;

        f.def("balanced-token")
            .alt(("(", "balanced-token-seq".opt(), ")"))
            .alt(("[", "balanced-token-seq".opt(), "]"))
            .alt(("{", "balanced-token-seq".opt(), "}"))
            .alt("token-not-bracket")
        ;

        // Modules

        f.def("module-declaration")
            .alt(("export-keyword".opt(), "module-keyword", "module-name", "module-partition".opt(), "attribute-specifier-seq".opt(), ";"));

        f.def("module-name")
            .alt(("module-name-qualifier".opt(), "identifier"));

        f.def("module-partition")
            .alt((":", "module-name-qualifier".opt(), "identifier"));

        f.def("module-name-qualifier")
            .alt(("identifier", "."))
            .alt(("module-name-qualifier", "identifier", "."))
        ;

        f.def("export-declaration")
            .alt(("export", "name-declaration"))
            .alt(("export", "{", "declaration-seq".opt(), "}"))
            .alt(("export-keyword", "module-import-declaration"))
        ;

        f.def("module-import-declaration")
            .alt(("import-keyword", "module-name", "attribute-specifier-seq".opt(), ";"))
            .alt(("import-keyword", "module-partition", "attribute-specifier-seq".opt(), ";"))
            .alt(("import-keyword", "header-name", "attribute-specifier-seq".opt(), ";"))
        ;

        f.def("global-module-fragment")
            .alt(("module-keyword", ";", "declaration-seq".opt()));

        f.def("private-module-fragment")
            .alt(("module-keyword", ":", "private", ";", "declaration-seq".opt()));

        // Classes

        f.def("class-name")
            .alt("identifier")
            .alt("simple-template-id")
        ;

        f.def("class-specifier")
            .alt(("class-head", "{", "member-specification".opt(), "}"))
        ;

        f.def("class-head")
            .alt(("class-key", "attribute-specifier-seq".opt(), "class-head-name", "class-virt-specifier".opt(), "base-clause".opt()))
            .alt(("class-key", "attribute-specifier-seq".opt(), "base-clause".opt()))
        ;

        f.def("class-head-name")
            .alt(("nested-name-specifier".opt(), "class-name"));

        f.def("class-virt-specifier")
            .alt("final");

        f.def("class-key")
            .alt("class")
            .alt("struct")
            .alt("union")
        ;

        f.def("member-specification")
            .alt(("member-declaration", "member-specification".opt()))
            .alt(("access-specifier", ":", "member-specification".opt()))
        ;

        f.def("member-declaration")
            .alt(("attribute-specifier-seq".opt(), "decl-specifier-seq".opt(), "member-declarator-list".opt(), ";"))
            .alt("function-definition")
            .alt("using-declaration")
            .alt("using-enum-declaration")
            .alt("static_assert-declaration")
            .alt("template-declaration")
            .alt("explicit-specialization")
            .alt("deduction-guide")
            .alt("alias-declaration")
            .alt("opaque-enum-declaration")
            .alt("empty-declaration")
        ;

        f.def("member-declarator-list")
            .alt("member-declarator")
            .alt(("member-declarator-list", ",", "member-declarator"))
        ;

        f.def("member-declarator")
            .alt(("declarator", "virt-specifier-seq".opt(), "pure-specifier".opt()))
            .alt(("declarator", "requires-clause"))
            .alt(("declarator", "brace-or-equal-initializer".opt()))
            .alt(("identifier".opt(), "attribute-specifier-seq".opt(), ":", "constant-expression", "brace-or-equal-initializer".opt()))
        ;

        f.def("virt-specifier-seq")
            .alt("virt-specifier")
            .alt(("virt-specifier-seq", "virt-specifier"))
        ;

        f.def("virt-specifier")
            .alt("override")
            .alt("final")
        ;

        f.def("pure-specifier")
            .alt(("=", "0"));

        f.def("conversion-function-id")
            .alt(("operator", "conversion-type-id"));

        f.def("conversion-type-id")
            .alt(("type-specifier-seq", "conversion-declarator".opt()));

        f.def("conversion-declarator")
            .alt(("ptr-operator", "conversion-declarator".opt()));

        f.def("base-clause")
            .alt((":", "base-specifier-list"));

        f.def("base-specifier-list")
            .alt(("base-specifier", "...".opt()))
            .alt(("base-specifier-list", ",", "base-specifier", "...".opt()))
        ;

        f.def("base-specifier")
            .alt(("attribute-specifier-seq", "class-or-decltype"))
            .alt(("attribute-specifier-seq".opt(), "virtual", "access-specifier".opt(), "class-or-decltype"))
            .alt(("attribute-specifier-seq".opt(), "access-specifier", "virtual".opt(), "class-or-decltype"))
        ;

        f.def("class-or-decltype")
            .alt(("nested-name-specifier".opt(), "type-name"))
            .alt(("nested-name-specifier", "template", "simple-template-id"))
            .alt("decltype-specifier")
        ;

        f.def("access-specifier")
            .alt("private")
            .alt("protected")
            .alt("public")
        ;

        f.def("ctor-initializer")
            .alt((":", "mem-initializer-list"));

        f.def("mem-initializer-list")
            .alt(("mem-initializer", "...".opt()))
            .alt(("mem-initializer-list", ",", "mem-initializer", "...".opt()))
        ;

        f.def("mem-initializer")
            .alt(("mem-initializer-id", "(", "expression-list".opt(), ")"))
            .alt(("mem-initializer-id", "braced-init-list"))
        ;

        f.def("mem-initializer-id")
            .alt("class-or-decltype")
            .alt("identifier")
        ;

        // Overloading

        f.def("operator-function-id")
            .alt(("operator", "token-operator"));

        f.def("token-operator")
            .alt("new").alt("delete").alt(("new", "[", "]")).alt(("delete", "[", "]")).alt("co_await").alt(("(", ")")).alt(("[", "]")).alt("->").alt("->*")
            .alt("~").alt("!").alt("+").alt("-").alt("*").alt("/").alt("%").alt("^").alt("&")
            .alt("|").alt("=").alt("+=").alt("-=").alt("*=").alt("/=").alt("%=").alt("^=").alt("&=")
            .alt("|=").alt("==").alt("!=").alt("<").alt(">").alt("<=").alt(">=").alt("<=>").alt("&&")
            .alt("||").alt("<<").alt(">>").alt("<<=").alt(">>=").alt("++").alt("--").alt(",")
        ;

        f.def("literal-operator-id")
            .alt(("operator", "string-literal", "identifier"))
            .alt(("operator", "user-defined-string-literal"))
        ;

        // Templates

        f.def("template-declaration")
            .alt(("template-head", "declaration"))
            .alt(("template-head", "concept-definition"))
        ;

        f.def("template-head")
            .alt(("template", "<", "template-parameter-list", ">", "requires-clause".opt()));

        f.def("template-parameter-list")
            .alt("template-parameter")
            .alt(("template-parameter-list", ",", "template-parameter"))
        ;

        f.def("requires-clause")
            .alt(("requires", "constraint-logical-or-expression"));

        f.def("constraint-logical-or-expression")
            .alt("constraint-logical-and-expression")
            .alt(("constraint-logical-or-expression", "||", "constraint-logical-and-expression"))
        ;

        f.def("constraint-logical-and-expression")
            .alt("primary-expression")
            .alt(("constraint-logical-and-expression", "&&", "primary-expression"))
        ;

        f.def("template-parameter")
            .alt("type-parameter")
            .alt("parameter-declaration")
        ;

        f.def("type-parameter")
            .alt(("type-parameter-key", "...".opt(), "identifier".opt()))
            .alt(("type-parameter-key", "identifier".opt(), "=", "type-id"))
            .alt(("type-constraint", "...".opt(), "identifier".opt()))
            .alt(("type-constraint", "identifier".opt(), "=", "type-id"))
            .alt(("template-head", "type-parameter-key", "...".opt(), "identifier".opt()))
            .alt(("template-head", "type-parameter-key", "identifier".opt(), "=", "id-expression"))
        ;

        f.def("type-parameter-key")
            .alt("class")
            .alt("typename")
        ;

        f.def("type-constraint")
            .alt(("nested-name-specifier".opt(), "concept-name"))
            .alt(("nested-name-specifier".opt(), "concept-name", "<", "template-argument-list".opt(), ">"))
        ;

        f.def("simple-template-id")
            .alt(("template-name", "<", "template-argument-list".opt(), ">"));

        f.def("template-id")
            .alt("simple-template-id")
            .alt(("operator-function-id", "<", "template-argument-list".opt(), ">"))
            .alt(("literal-operator-id", "<", "template-argument-list".opt(), ">"))
        ;

        f.def("template-name")
            .alt("identifier");

        f.def("template-argument-list")
            .alt(("template-argument", "...".opt()))
            .alt(("template-argument-list", ",", "template-argument", "...".opt()))
        ;

        f.def("template-argument")
            .alt("constant-expression")
            .alt("type-id")
            .alt("id-expression")
        ;

        f.def("constraint-expression")
            .alt("logical-or-expression");

        f.def("deduction-guide")
            .alt(("explicit-specifier".opt(), "template-name", "(", "parameter-declaration-clause", ")", "->", "simple-template-id", ";"));

        f.def("concept-definition")
            .alt(("concept", "concept-name", "attribute-specifier-seq".opt(), "=", "constraint-expression", ";"));

        f.def("concept-name")
            .alt("identifier");

        f.def("typename-specifier")
            .alt(("typename", "nested-name-specifier", "identifier"))
            .alt(("typename", "nested-name-specifier", "template".opt(), "simple-template-id"))
        ;

        f.def("explicit-instantiation")
            .alt(("extern".opt(), "template", "declaration"));

        f.def("explicit-specialization")
            .alt(("template", "<", ">", "declaration"));

        // Exception handling

        f.def("try-block")
            .alt(("try", "compound-statement", "handler-seq"));

        f.def("function-try-block")
            .alt(("try", "ctor-initializer".opt(), "compound-statement", "handler-seq"));

        f.def("handler-seq")
            .alt(("handler", "handler-seq".opt()));

        f.def("handler")
            .alt(("catch", "(", "exception-declaration", ")", "compound-statement"));

        f.def("exception-declaration")
            .alt(("attribute-specifier-seq".opt(), "type-specifier-seq", "declarator"))
            .alt(("attribute-specifier-seq".opt(), "type-specifier-seq", "abstract-declarator".opt()))
            .alt("...")
        ;

        f.def("noexcept-specifier")
            .alt(("noexcept", "(", "constant-expression", ")"))
            .alt("noexcept")
        ;

        // Preprocessing directives

        f.def("identifier-list")
            .alt("identifier")
            .alt(("identifier-list", ",", "identifier"))
        ;
    }

    fn walk_terminals(&self, f: &mut impl VisitTerminals) {
        // Preprocessing tokens

        f
            .token("end-of-file").token("end-of-line")

            .token("header-name")

            .token("identifier").token("number").token("character").token("string")

            .token("#").token("##")

            .token("{").token("}").token("[").token("]").token("(").token(")")
            .token(";").token(":").token("...")
            .token("?").token("::").token(".").token(".*").token("->").token("->*").token("~")
            .token("!").token("+").token("-").token("*").token("/").token("%").token("^").token("&").token("|")
            .token("=").token("+=").token("-=").token("*=").token("/=").token("%=").token("^=").token("&=").token("|=")
            .token("==").token("!=").token("<").token(">").token("<=").token(">=").token("<=>").token("&&").token("||")
            .token("<<").token(">>").token("<<=").token(">>=").token("++").token("--").token(",")

            .token("error")
        ;

        // Additional phase 7 tokens

        f
            .token("integer-literal")
            .token("character-literal")
            .token("floating-point-literal")
            .token("string-literal")
            .token("user-defined-integer-literal")
            .token("user-defined-floating-point-literal")
            .token("user-defined-string-literal")
            .token("user-defined-character-literal")
        ;

        f
            .token("alignas").token("alignof").token("asm").token("auto").token("bool")
            .token("break").token("case").token("catch").token("char").token("char8_t")
            .token("char16_t").token("char32_t").token("class").token("concept").token("const")
            .token("consteval").token("constexpr").token("constinit").token("const_cast")
            .token("continue").token("co_await").token("co_return").token("co_yield")
            .token("decltype").token("default").token("delete").token("do").token("double")
            .token("dynamic_cast").token("else").token("enum").token("explicit").token("export")
            .token("extern").token("false").token("float").token("for").token("friend")
            .token("goto").token("if").token("inline").token("int").token("long").token("mutable")
            .token("namespace").token("new").token("noexcept").token("nullptr").token("operator")
            .token("private").token("protected").token("public").token("register")
            .token("reinterpret_cast").token("requires").token("return").token("short")
            .token("signed").token("sizeof").token("static").token("static_assert")
            .token("static_cast").token("struct").token("switch").token("template").token("this")
            .token("thread_local").token("throw").token("true").token("try").token("typedef")
            .token("typeid").token("typename").token("union").token("unsigned").token("using")
            .token("virtual").token("void").token("volatile").token("wchar_t").token("while")
            .token("import-keyword").token("module-keyword").token("export-keyword")
        ;
    }
}
