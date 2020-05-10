#![allow(dead_code)]
#[allow(unused_variables)]
use std::env;
use std::fs;

type UpperTerm = String;

trait PrintAsDDLog {
    fn to_ddlog_type(&self, enums: &Enums) -> String;
}

impl PrintAsDDLog for UpperTerm {
    fn to_ddlog_type(&self, enums: &Enums) -> String {
        let values = enums.into_iter().map(|e| e.name.clone()).collect::<Vec<String>>();
        if values.contains(self) {
            self.clone()
        } else {
            match self.as_str() {
                "Booleans" => "bool".to_string(),
                "Integers" => "s64".to_string(),
                _ => "OID".to_string(),
            }
        }
    }
}

enum Term {
    TermUpper(UpperTerm),
    TermInteger(i64),
}

type Variable = String;

enum Expression {
    ExpressionTerm(Term),
    ExpressionVariable(Variable),
}

struct Enum {
    name: UpperTerm,
    terms: Vec<UpperTerm>,
}

#[derive(Clone)]
struct Sort {
    name: UpperTerm,
    parent_sorts: Vec<UpperTerm>,
    attributes: Option<FunctionDeclarations>,
}

#[derive(Clone)]
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
    ClauseFunctionAssignment(FunctionAssignment),
    ClauseRawDDLog(RawDDLog),
}

enum Axiom {
    StaticAssignment {
        name: UpperTerm,
        value: i64,
    },
    Fact(FunctionAssignment),
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

fn print_enum(enums: Enums) -> String {
    enums
        .into_iter()
        .map(|e| format!("typedef {} = {}", e.name, e.terms.join(" | ")))
        .collect::<Vec<String>>()
        .join("\n")
}

fn print_nodes(sorts: Sorts) -> String {
    let defaults = vec!["Universe".to_string(), "Actions".to_string()];
    let rest = sorts.into_iter().map(|s| s.name).collect::<Vec<String>>();
    let all = vec![defaults, rest].concat();
    print_ddlog_enum("Node", all)
}

fn print_ddlog_enum(name: &str, mut members: Vec<String>) -> String {
    members.reverse();
    let first = members.pop().expect("Can't create emtpy enum");
    members.reverse();
    let preamble = format!("typedef {} = {}", name, first);
    if members.len() > 0 {
        let mut preamble_owned = preamble.to_owned();
        preamble_owned.push_str("\n    | ");
        let rest = members.join("\n    | ").to_owned();
        preamble_owned.push_str(&rest);
        preamble_owned
    } else {
        preamble
    }
}

fn print_attribute_values(enums: &Enums, sorts: Sorts) -> String {
    let rest = sorts
        .into_iter()
        .filter_map(|s| {
            s.attributes.map(|attributes| {
                attributes
                    .into_iter()
                    .map(|f| {
                        format!(
                            "Attr_{}{{{}: {}}}",
                            f.name,
                            f.name.to_lowercase(),
                            f.ret.to_ddlog_type(enums)
                        )
                    })
                    .collect::<Vec<String>>()
            })
        })
        .collect::<Vec<Vec<String>>>()
        .concat();
    print_ddlog_enum("AttributeValue", rest)
}

fn print_attribute_relations(enums: &Enums, sorts: Sorts) -> String {
    sorts
        .into_iter()
        .filter_map(|s| {
            s.attributes.map(|attributes| {
                attributes
                    .into_iter()
                    .map(|f| {
                        format!(
                            "relation {}(oid: OID, value: {})
{}(oid, x) :- Object(oid, _, attributes),
    Some{{Attr_{}{{var x}}}} = map_get(attributes, \"{}\").",
                            f.name,
                            f.ret.to_ddlog_type(enums),
                            f.name,
                            f.name,
                            f.name.to_lowercase()
                        )
                    })
                    .collect::<Vec<String>>()
            })
        })
        .collect::<Vec<Vec<String>>>()
        .concat()
        .join("\n\n")
}

fn print_links(sorts: Sorts) -> String {
    sorts
        .into_iter()
        .flat_map(|s| {
            s.clone()
                .parent_sorts
                .into_iter()
                .map(move |parent| format!("Link({}, {}).", s.name, parent.clone()))
        })
        .collect::<Vec<String>>()
        .join("\n")
}

fn print_static_declarations(statics: Statics) -> String {
    statics
        .into_iter()
        .filter(|s| match s.params {
            Some(_) => true,
            None => false,
        })
        .map(|s| {
            let mut param_str = match s.params {
                Some(params) => {
                    let mut v: Vec<String> = Vec::new();
                    for i in 0..params.len() {
                        let p = format!("_{}: {}", i + 1, params[i]);
                        v.push(p);
                    }
                    v
                }
                None => Vec::new(),
            };

            let ret = format!("ret: {}", s.ret);
            let all = if param_str.len() > 0 {
                param_str.push(ret);
                param_str.join(", ")
            } else {
                ret
            };
            format!("relation {}({})", s.name, all)
        })
        .collect::<Vec<String>>()
        .join("\n\n")
}

fn print_basic_fluent_params(enums: &Enums, basic_fluents: FunctionDeclarations) -> String {
    let types = basic_fluents
        .into_iter()
        .map(|f| {
            let lower_case = f.name.to_lowercase();
            let param_vec = match f.params {
                Some(params) => {
                    let mut v: Vec<String> = Vec::new();
                    for i in 0..params.len() {
                        let p = format!(
                            "{}_{}: {}",
                            lower_case,
                            i + 1,
                            params[i].to_ddlog_type(enums)
                        );
                        v.push(p);
                    }
                    v
                }
                None => Vec::new(),
            };
            format!("Param_{}{{{}}}", f.name, param_vec.join(", "))
        })
        .collect();
    print_ddlog_enum("FluentParam", types)
}

fn print_basic_fluent_values(enums: &Enums, basic_fluents: FunctionDeclarations) -> String {
    let types = basic_fluents
        .into_iter()
        .map(|f| {
            format!(
                "Value_{}{{{}_ret: {}}}",
                f.name,
                f.name.to_lowercase(),
                f.ret.to_ddlog_type(enums)
            )
        })
        .collect();
    print_ddlog_enum("FluentValue", types)
}

fn print_defined_fluent_relations(
    enums: &Enums,
    defined_fluents: DefinedFluentDeclarations,
) -> String {
    defined_fluents
        .into_iter()
        .map(|dec|{
            let d = dec.declaration;
            let param_vec = match d.params {
                Some(params) => {
                    let mut v: Vec<String> = Vec::new();
                    for i in 0..params.len() {
                        let p = format!(
                            "_{}: {}",
                            i + 1,
                            params[i].to_ddlog_type(enums)
                        );
                        v.push(p);
                    }
                    v
                }
                None => Vec::new(),
            };
            format!("relation {}({})", d.name, param_vec.join(", "))
        }).collect::<Vec<String>>()
        .join("\n")
}

fn print_output_relations(defined_fluents: DefinedFluentDeclarations) -> String {
    let mut str = "#[rust=\"serde(untagged)\"]\n".to_owned();
    let output_relations = defined_fluents
        .into_iter()
        .filter(|dec| dec.output)
        .map(|dec| {
            let name = dec.declaration.name;
            format!("Out_{}{{{}: {}}}", name, name.to_lowercase(), name)
        }).collect::<Vec<String>>();
    let typedef = print_ddlog_enum("Output_Value", output_relations);
    str.push_str(typedef.as_str());
    str
}

fn print_output_rules(defined_fluents: DefinedFluentDeclarations) -> String {
    defined_fluents
        .into_iter()
        .filter(|dec| dec.output)
        .map(|dec| {
            let name = dec.declaration.name;
            format!("Output(Out_{}{{{}}}) :- {}[{}].", name, name.to_lowercase(), name, name.to_lowercase())
        }).collect::<Vec<String>>()
        .join("\n")
}

fn print_axiom(axiom: Axiom) -> String {
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
                name: "Rectangles".to_string(),
                parent_sorts: vec!["Universe".to_string()],
                attributes: Some(vec![
                    FunctionDeclaration {
                        name: "Width".to_string(),
                        params: None,
                        ret: "Integers".to_string(),
                    },
                    FunctionDeclaration {
                        name: "Height".to_string(),
                        params: None,
                        ret: "Integers".to_string(),
                    },
                ]),
            },
            Sort {
                name: "Windows".to_string(),
                parent_sorts: vec!["Rectangles".to_string()],
                attributes: None,
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
            print_attribute_values(&Vec::new(), make_sorts()),
            "typedef AttributeValue = Attr_Width{width: s64}
    | Attr_Height{height: s64}"
        );
    }

    #[test]
    fn printing_attribute_relations() {
        assert_eq!(
            print_attribute_relations(&Vec::new(), make_sorts()),
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
            "relation Opposite_Directions(_1: Directions, ret: Directions)"
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
    fn printing_basic_fluent_params() {
        assert_eq!(
            print_basic_fluent_params(&Vec::new(), make_basic_fluent_declarations()),
            "typedef FluentParam = Param_Grouped_With{grouped_with_1: OID, grouped_with_2: OID}
    | Param_Moving{moving_1: OID}"
        )
    }

    #[test]
    fn printing_basic_fluent_values() {
        assert_eq!(
            print_basic_fluent_values(&Vec::new(), make_basic_fluent_declarations()),
            "typedef FluentValue = Value_Grouped_With{grouped_with_ret: bool}
    | Value_Moving{moving_ret: bool}"
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
                        "Windows".to_string(),
                        "Axes".to_string(),
                        "Integers".to_string(),
                    ]),
                    ret: "Booleans".to_string(),
                },
            },
            DefinedFluentDeclaration {
                output: false,
                declaration: FunctionDeclaration {
                    name: "Distance".to_string(),
                    params: Some(vec![
                        "Windows".to_string(),
                        "Windows".to_string(),
                        "Integers".to_string(),
                    ]),
                    ret: "Booleans".to_string(),
                },
            },
        ]
    }

    #[test]
    fn printing_defined_fluent_relations() {
        let enums = vec![Enum {
            name: "Axes".to_string(),
            terms: vec!["X".to_string(), "Y".to_string()],
        }];
        assert_eq!(
            print_defined_fluent_relations(&enums, make_defined_fluent_declarations()),
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
                "Output(Out_Side{side}) :- Side[side]."
            )
        }
    //     #[test]
    //     fn printing_static_function_assignment() {
    //         let static_assignment = Axiom::StaticAssignment {
    //             name: "Corner_Snapping_Threshold".to_string(),
    //             value: 30,
    //         };
    //         assert_eq!(
    //             print_axiom(static_assignment),
    //             "function static_Corner_Snapping_Threshold(): s64 { 30 }"
    //         )
    //     }

    //     #[test]
    //     fn printing_facts() {
    //         let fact = Axiom::Fact(FunctionAssignment {
    //             name: "Distance".to_string(),
    //             negated: false,
    //             arguments: vec![
    //                 Expression::ExpressionTerm(Term::TermInteger(1)),
    //                 Expression::ExpressionTerm(Term::TermInteger(2)),
    //                 Expression::ExpressionTerm(Term::TermInteger(10)),
    //             ],
    //             value: None,
    //         });
    //         assert_eq!(print_axiom(fact), "Distance(1, 2, 10).")
    //     }

    //     #[test]
    //     fn printing_rules() {
    //         let rule = Axiom::Rule {
    //             head: FunctionAssignment {
    //                 name: "Distance".to_string(),
    //                 negated: false,
    //                 arguments: vec![
    //                     Expression::ExpressionVariable("a".to_string()),
    //                     Expression::ExpressionVariable("b".to_string()),
    //                     Expression::ExpressionVariable("min_d".to_string()),
    //                 ],
    //                 value: None,
    //             },
    //             body: vec![
    //                 RuleClause::ClauseFunctionAssignment(FunctionAssignment {
    //                     name: "Instance".to_string(),
    //                     negated: false,
    //                     arguments: vec![
    //                         Expression::ExpressionVariable("a".to_string()),
    //                         Expression::ExpressionTerm(Term::TermUpper("Rectangles".to_string())),
    //                     ],
    //                     value: None,
    //                 }),
    //                 RuleClause::ClauseFunctionAssignment(FunctionAssignment {
    //                     name: "Instance".to_string(),
    //                     negated: false,
    //                     arguments: vec![
    //                         Expression::ExpressionVariable("b".to_string()),
    //                         Expression::ExpressionTerm(Term::TermUpper("Rectangles".to_string())),
    //                     ],
    //                     value: None,
    //                 }),
    //                 RuleClause::ClauseFunctionAssignment(FunctionAssignment {
    //                     name: "Overlaps".to_string(),
    //                     negated: true,
    //                     arguments: vec![
    //                         Expression::ExpressionVariable("a".to_string()),
    //                         Expression::ExpressionVariable("b".to_string()),
    //                     ],
    //                     value: None,
    //                 }),
    //                 RuleClause::ClauseFunctionAssignment(FunctionAssignment {
    //                     name: "Opposite_Direction".to_string(),
    //                     negated: false,
    //                     arguments: vec![
    //                         Expression::ExpressionVariable("dir".to_string()),
    //                     ],
    //                     value: Some(Expression::ExpressionVariable("dir__prime".to_string())),
    //                 }),
    //                 RuleClause::ClauseRawDDLog("var min_d = Aggregate((a, b), group_min(b)).".to_string())
    //             ],
    //         };

    //         assert_eq!(
    //             print_axiom(rule),
    //             "Distance(a, b, min_d) :-
    //     Instance(a, Rectangles),
    //     Instance(b, Rectangles),
    //     not Overlaps(a, b),
    //     Opposite_Direction(dir, dir__prime),
    //     var min_d = Aggregate((a, b), group_min(b)).
    // "
    //         )
    //     }
}
