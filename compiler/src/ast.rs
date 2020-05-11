use std::fmt::{Debug};

pub type UpperTerm = String;

#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    TermUpper(UpperTerm),
    TermInteger(i64),
    TermBoolean(bool),
}
pub type Variable = String;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    ExpressionTerm(Term),
    ExpressionVariable(Variable),
}

#[derive(PartialEq, Debug)]
pub struct Enum {
    pub name: UpperTerm,
    pub terms: Vec<UpperTerm>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Sort {
    pub name: UpperTerm,
    pub parent_sorts: Vec<UpperTerm>,
    pub attributes: Option<FunctionDeclarations>,
}

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionDeclaration {
    pub name: UpperTerm,
    pub params: Option<Vec<UpperTerm>>,
    pub ret: UpperTerm,
}

pub type FunctionDeclarations = Vec<FunctionDeclaration>;
#[derive(PartialEq, Debug)]
pub struct DefinedFluentDeclaration {
    pub output: bool,
    pub declaration: FunctionDeclaration,
}

pub type DefinedFluentDeclarations = Vec<DefinedFluentDeclaration>;

#[derive(PartialEq, Debug)]
pub struct Fluents {
    pub basic: FunctionDeclarations,
    pub defined: DefinedFluentDeclarations,
}

#[derive(PartialEq, Debug)]
pub struct FunctionAssignment {
    pub name: UpperTerm,
    pub arguments: Vec<Expression>,
    pub value: Option<Expression>,
    pub negated: bool,
}

#[derive(PartialEq, Debug)]
pub enum FunctionAssignmentKinds {
    InFluent,
    OutFluent,
    Normal,
}

pub type RawDDLog = String;

#[derive(PartialEq, Debug)]
pub enum RuleClause {
    ClauseFunctionAssignment(FunctionAssignment),
    ClauseRawDDLog(RawDDLog),
}
#[derive(PartialEq, Debug)]
pub enum Axiom {
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

pub type Enums = Vec<Enum>;
pub type Sorts = Vec<Sort>;
pub type Statics = FunctionDeclarations;
pub type Axioms = Vec<Axiom>;

#[derive(PartialEq, Debug)]
pub struct ALMModule {
    pub enums: Enums,
    pub sorts: Sorts,
    pub statics: Statics,
    pub fluents: Fluents,
    pub axioms: Axioms,
}
