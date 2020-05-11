pub type UpperTerm = String;

#[derive(Clone)]
pub enum Term {
    TermUpper(UpperTerm),
    TermInteger(i64),
    TermBoolean(bool),
}

pub type Variable = String;

#[derive(Clone)]
pub enum Expression {
    ExpressionTerm(Term),
    ExpressionVariable(Variable),
}

pub struct Enum {
    pub name: UpperTerm,
    pub terms: Vec<UpperTerm>,
}

#[derive(Clone)]
pub struct Sort {
    pub name: UpperTerm,
    pub parent_sorts: Vec<UpperTerm>,
    pub attributes: Option<FunctionDeclarations>,
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub name: UpperTerm,
    pub params: Option<Vec<UpperTerm>>,
    pub ret: UpperTerm,
}

pub type FunctionDeclarations = Vec<FunctionDeclaration>;

pub struct DefinedFluentDeclaration {
    pub output: bool,
    pub declaration: FunctionDeclaration,
}

pub type DefinedFluentDeclarations = Vec<DefinedFluentDeclaration>;

pub struct Fluents {
    pub basic: FunctionDeclarations,
    pub defined: DefinedFluentDeclarations,
}

pub struct FunctionAssignment {
    pub name: UpperTerm,
    pub arguments: Vec<Expression>,
    pub value: Option<Expression>,
    pub negated: bool,
}

#[derive(PartialEq)]
pub enum FunctionAssignmentKinds {
    InFluent,
    OutFluent,
    Normal,
}

pub type RawDDLog = String;

pub enum RuleClause {
    ClauseFunctionAssignment(FunctionAssignment),
    ClauseRawDDLog(RawDDLog),
}

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

pub struct ALMModule {
    pub enums: Enums,
    pub sorts: Sorts,
    pub statics: Statics,
    pub fluents: Fluents,
    pub axioms: Axioms,
}
