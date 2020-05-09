use std::env;
use std::fs;

type UpperTerm = String;

enum Term {
    TermUpper { upper: UpperTerm },
    TermInteger { integer: i64 },
}

type Variable = String;

enum Expression {
    ExpressionTerm { term: Term },
    ExpressionVariable { variable: Variable },
}

struct Enum {
    name: UpperTerm,
    terms: Vec<UpperTerm>,
}

struct Sort {
    name: UpperTerm,
    subsort: UpperTerm,
    attributes: Option<FunctionDeclarations>,
}

struct FunctionDeclaration {
    name: UpperTerm,
    params: Option<Vec<UpperTerm>>,
    ret: UpperTerm,
}

type FunctionDeclarations = Vec<FunctionDeclaration>;

struct DefinedFluentDeclaration {
    output: bool,
    declaration: FunctionDeclaration,
}

type DefinedFluentDeclarations = Vec<DefinedFluentDeclaration>;

struct Fluents {
    basic: FunctionDeclarations,
    defined: DefinedFluentDeclarations,
}

struct FunctionAssignment {
    name: UpperTerm,
    arguments: Vec<Expression>,
    value: Option<Expression>,
    negated: bool,
}

type RawDDLog = String;

enum RuleClause {
    ClauseFunctionAssignment {
        function_assignment: FunctionAssignment,
    },
    ClauseRawDDLog {
        raw_ddlog: RawDDLog,
    },
}

enum Axiom {
    StaticAssignment {
        name: UpperTerm,
    },
    Fact {
        fact: FunctionAssignment,
    },
    Rule {
        head: FunctionAssignment,
        body: Vec<RuleClause>,
    },
}

type Enums = Vec<Enum>;
type Sorts = Vec<Sort>;
type Statics = FunctionDeclarations;
type Axioms = Vec<Axiom>;

struct ALMModule {
    enums: Enums,
    sorts: Sorts,
    statics: Statics,
    fluents: Fluents,
    axioms: Axioms,
}

fn join_enums(preamble: String, enums: Vec<String>) -> String {
    unimplemented!()
}

fn print_enum(enums: Enums) -> String {
    enums
        .into_iter()
        .map(|e| format!("typedef {} = {}", e.name, e.terms.join(" | ")))
        .collect::<Vec<String>>()
        .join("\n")
}

fn print_nodes(sorts: Sorts) -> String {
    let mut str = "typedef Node = Universe
    | Actions
    | "
    .to_owned();
    let rest = sorts
        .into_iter()
        .map(|s| s.name)
        .collect::<Vec<String>>()
        .join("\n    | ")
        .to_owned();
    str.push_str(&rest);
    str
}

fn print_attribute_values(sorts: Sorts) -> String {
    unimplemented!();
}

fn print_attribute_relations(sorts: Sorts) -> String {
    unimplemented!();
}

fn print_links(sorts: Sorts) -> String {
    unimplemented!();
}

fn print_static_declarations(statics: Statics) -> String {
    unimplemented!();
}

fn print_basic_fluent_params(basic_fluents: FunctionDeclarations) -> String {
    unimplemented!();
}

fn print_basic_fluent_values(basic_fluents: FunctionDeclarations) -> String {
    unimplemented!();
}

fn print_defined_fluent_relations(defined_fluents: DefinedFluentDeclarations) -> String {
    unimplemented!();
}
fn print_output_relations(defined_fluents: DefinedFluentDeclarations) -> String {
    unimplemented!();
}

fn print_output_rules(defined_fluents: DefinedFluentDeclarations) -> String {
    unimplemented!();
}

fn main() {
    let alm_file = env::args()
        .nth(1)
        .expect("Please specify an ALM module to compile.");

    let read_file_error = format!("Could not read file {}!", alm_file);
    let contents = fs::read_to_string(alm_file).expect(read_file_error.as_str());

    println!("{}", contents);
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn printing_enums() {
        let enums = vec![Enum {
            name: "Axes".to_string(),
            terms: vec!["X".to_string(), "Y".to_string()],
        }];
        assert_eq!(print_enum(enums), "typedef Axes = X | Y")
    }

    fn make_sorts() -> Sorts {
        vec![
            Sort {
                name: "Windows".to_string(),
                subsort: "Rectangles".to_string(),
                attributes: None,
            },
            Sort {
                name: "Rectangles".to_string(),
                subsort: "Universe".to_string(),
                attributes: Some(vec![
                    FunctionDeclaration {
                        name: "Width".to_string(),
                        params: None,
                        ret: "s64".to_string(),
                    },
                    FunctionDeclaration {
                        name: "Height".to_string(),
                        params: None,
                        ret: "s64".to_string(),
                    },
                ]),
            },
        ]
    }

    #[test]
    fn printing_nodes() {
        assert_eq!(
            print_nodes(make_sorts()),
            "typedef Node = Universe
    | Actions
    | Rectangles
    | Windows"
        );
    }
    #[test]
    fn printing_attribute_values() {
        assert_eq!(
            print_attribute_values(make_sorts()),
            "typedef AttributeValue = Attr_Width{width: s64}
    | Attr_Height{height: s64}"
        );
    }

    #[test]
    fn printing_attribute_relations() {
        assert_eq!(
            print_attribute_relations(make_sorts()),
            "relation Width(oid: OID, value: s64)
Width(oid, x) :- Object(oid, _, attributes),
    Some{Attr_Width{var x}} = map_get(attributes, \"width\").

relation Height(oid: OID, value: s64)
Height(oid, x) :- Object(oid, _, attributes),
    Some{Attr_Height{var x}} = map_get(attributes, \"height\")."
        );
    }

    #[test]
    fn printing_links() {
        assert_eq!(
            print_links(make_sorts()),
            "Link(Rectangles, Universe).
Link(Windows, Rectangles)."
        )
    }

    #[test]
    fn printing_static_declarations() {
        let static_declarations = vec![
            FunctionDeclaration {
                name: "Opposite_Directions".to_string(),
                params: Some(vec!["Directions".to_string()]),
                ret: "Directions".to_string(),
            },
            FunctionDeclaration {
                name: "Snapping_Threshold".to_string(),
                params: None,
                ret: "s64".to_string(),
            },
        ];
        assert_eq!(
            print_static_declarations(static_declarations),
            "relation Opposite_Directions(_1: Directions, _ret: Directions)"
        )
    }

    fn make_basic_fluent_declarations() -> FunctionDeclarations {
        vec![
            FunctionDeclaration {
                name: "Grouped_With".to_string(),
                params: Some(vec!["Windows".to_string(), "Windows".to_string()]),
                ret: "Booleans".to_string(),
            },
            FunctionDeclaration {
                name: "Moving".to_string(),
                params: Some(vec!["Windows".to_string()]),
                ret: "Booleans".to_string(),
            },
        ]
    }

    #[test]
    fn printing_basic_fluent_types() {
        assert_eq!(
            print_basic_fluent_params(make_basic_fluent_declarations()),
            "typedef FluentParam = Param_Grouped_With{grouped_with_1: OID, grouped_with_2: OID}
    | Param_Moving{moving_1: OID}"
        )
    }

    #[test]
    fn printing_basic_fluent_values() {
        assert_eq!(
            print_basic_fluent_params(make_basic_fluent_declarations()),
            "typedef FluentValue = Value_Grouped_With{grouped_with_ret: bool}
    | Value_Moving{moving_ret}"
                .to_string(),
        )
    }

    fn make_defined_fluent_declarations() -> DefinedFluentDeclarations {
        vec![
            DefinedFluentDeclaration {
                output: true,
                declaration: FunctionDeclaration {
                    name: "Side".to_string(),
                    params: Some(vec![
                        "OID".to_string(),
                        "Axes".to_string(),
                        "s64".to_string(),
                    ]),
                    ret: "bool".to_string(),
                },
            },
            DefinedFluentDeclaration {
                output: false,
                declaration: FunctionDeclaration {
                    name: "Distance".to_string(),
                    params: Some(vec![
                        "OID".to_string(),
                        "OID".to_string(),
                        "s64".to_string(),
                    ]),
                    ret: "bool".to_string(),
                },
            },
        ]
    }

    #[test]
    fn printing_defined_fluent_relations() {
        assert_eq!(
            print_defined_fluent_relations(make_defined_fluent_declarations()),
            "relation Side(_1: OID, _2: Axes, _3: s64)
relation Distance(_1: OID, _2: OID, _3: s64)"
        )
    }

    #[test]
    fn printing_output_relations() {
        assert_eq!(
            print_output_relations(make_defined_fluent_declarations()),
            "#[rust=\"serde(untagged)\"]
typedef Output_Value = Out_Side{side: Side}"
        )
    }

    #[test]
    fn printing_output_rules() {
        assert_eq!(
            print_output_rules(make_defined_fluent_declarations()),
            "#[rust=\"serde(untagged)\"]
typedef Output_Value = Out_Side{side: Side}"
        )
    }
}
