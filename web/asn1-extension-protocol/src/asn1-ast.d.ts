export interface Loc {
    offset: number;
    length: number;
}

export interface AstElement<T> {
    element: T;
    loc: Loc;
}

export interface Soi {
    ast: 'Soi';
}

export interface Eoi {
    ast: 'Eoi';
}

export interface AstTypeReference {
    ast: 'TypeReference';
    item: string;
}

export interface AstUppercaseReference {
    ast: 'UppercaseReference';
    item: string;
}

export interface AstValueReference {
    ast: 'ValueReference';
    item: string;
}

export interface AstStringLiteral {
    ast: 'StringLiteral';
    item: {
        kind: 'hstring' | 'bstring' | 'cstring';
        data: string;
    };
}

export interface AstNumber {
    ast: 'Number';
    item: bigint;
}

export interface Keyword {
    ast: 'Keyword';
    item: string;
}

export interface Operator {
    ast: 'Operator';
    item: string;
}

export interface AstBracedTokenStream {
    ast: 'BracedTokenStream';
}

export interface AstSyntaxTokenLiteral {
    ast: 'SyntaxTokenLiteral';
}
export interface AstProgram {
    ast: 'Program';
    item: AstElement<AstModule>[];
}

export interface AstModule {
    ast: 'Module';
    header: AstElement<AstModuleHeader>;
    body: AstElement<AstModuleBody>;
}

export interface AstModuleHeader {
    ast: 'ModuleHeader';
    name: AstElement<AstTypeReference>;
    oid: AstElement<AstModuleIdentifier> | null;
    encoding_reference: AstElement<AstUppercaseReference> | null;
    tag_default: AstElement<AstTagDefault> | null;
    extensibility_implied: AstElement<AstExtensibilityImplied> | null;
    exports: AstElement<AstExports> | null;
    imports: AstElement<AstImports> | null;
}

export interface AstModuleBody {
    ast: 'ModuleBody';
    item: AstElement<AstAssignment>[];
}

export type AstModuleIdentifier = { ast: 'ModuleIdentifier'; } & ({
    variant: 'DefinitiveOidWithIri';
    value: AstElement<AstDefinitiveOidWithIri>;
} | {
    variant: 'DefinitiveOid';
    value: AstElement<AstDefinitiveOid>;
});

export interface AstDefinitiveOidWithIri {
    ast: 'DefinitiveOidWithIri';
    oid: AstElement<AstDefinitiveOid>;
    iri: AstElement<AstStringLiteral>;
}

export interface AstDefinitiveOid {
    ast: 'DefinitiveOid';
    item: AstElement<AstDefinitiveOidComponent>[];
}

export type AstDefinitiveOidComponent = { ast: 'DefinitiveOidComponent'; } & ({
    variant: 'NamedNumber';
    value: AstElement<AstNamedNumber>;
} | {
    variant: 'ValueReference';
    value: AstElement<AstValueReference>;
} | {
    variant: 'Number';
    value: AstElement<AstNumber>;
});

export interface AstNamedNumber {
    ast: 'NamedNumber';
    name: AstElement<AstValueReference>;
    num: AstElement<AstIntegerValueReference>;
}

export interface AstNamedNumberList {
    ast: 'NamedNumberList';
    item: AstElement<AstNamedNumber>[];
}

export interface AstTagDefault {
    ast: 'TagDefault';
    item: AstElement<AstTagDefaultKind>;
}

export interface AstTagDefaultExplicit {
    ast: 'TagDefaultExplicit';
}

export interface AstTagDefaultImplicit {
    ast: 'TagDefaultImplicit';
}

export interface AstTagDefaultAutomatic {
    ast: 'TagDefaultAutomatic';
}

export type AstTagDefaultKind = { ast: 'TagDefaultKind'; } & ({
    variant: 'TagDefaultExplicit';
    value: AstElement<AstTagDefaultExplicit>;
} | {
    variant: 'TagDefaultImplicit';
    value: AstElement<AstTagDefaultImplicit>;
} | {
    variant: 'TagDefaultAutomatic';
    value: AstElement<AstTagDefaultAutomatic>;
});

export interface AstExtensibilityImplied {
    ast: 'ExtensibilityImplied';
}

export interface AstExports {
    ast: 'Exports';
    item: AstElement<AstExportsKind> | null;
}

export interface AstAll {
    ast: 'All';
}

export type AstExportsKind = { ast: 'ExportsKind'; } & ({
    variant: 'All';
    value: AstElement<AstAll>;
} | {
    variant: 'SymbolList';
    value: AstElement<AstSymbolList>;
});

export interface AstImports {
    ast: 'Imports';
    item: AstElement<AstSymbolsFromModule>[];
}

export interface AstSymbolsFromModule {
    ast: 'SymbolsFromModule';
    symbols: AstElement<AstSymbolList>;
    module: AstElement<AstGlobalModuleReference>;
    selection_option: AstElement<AstSelectionOption> | null;
}

export interface AstSelectionOption {
    ast: 'SelectionOption';
    item: AstElement<AstSelectionOptionKind>;
}

export interface AstSuccessors {
    ast: 'Successors';
}

export interface AstDescendants {
    ast: 'Descendants';
}

export type AstSelectionOptionKind = { ast: 'SelectionOptionKind'; } & ({
    variant: 'Successors';
    value: AstElement<AstSuccessors>;
} | {
    variant: 'Descendants';
    value: AstElement<AstDescendants>;
});

export interface AstValueIdentifier {
    ast: 'ValueIdentifier';
    item: AstElement<AstDefinedValue>;
}

export type AstAssignedIdentifier = { ast: 'AssignedIdentifier'; } & ({
    variant: 'DefinitiveOid';
    value: AstElement<AstDefinitiveOid>;
} | {
    variant: 'ValueIdentifier';
    value: AstElement<AstValueIdentifier>;
});

export interface AstGlobalModuleReference {
    ast: 'GlobalModuleReference';
    name: AstElement<AstTypeReference>;
    oid: AstElement<AstAssignedIdentifier> | null;
}

export type AstOid = { ast: 'Oid'; } & ({
    variant: 'ObjectIdentifierValue';
    value: AstElement<AstObjectIdentifierValue>;
} | {
    variant: 'DefinedValue';
    value: AstElement<AstDefinedValue>;
});

export interface AstObjectIdentifierValue {
    ast: 'ObjectIdentifierValue';
    components: AstElement<AstObjectIdentifierComponent>[];
}

export type AstObjectIdentifierComponent = { ast: 'ObjectIdentifierComponent'; } & ({
    variant: 'NamedNumber';
    value: AstElement<AstNamedNumber>;
} | {
    variant: 'DefinedValue';
    value: AstElement<AstDefinedValue>;
} | {
    variant: 'Number';
    value: AstElement<AstNumber>;
});

export interface AstDefinedValue {
    ast: 'DefinedValue';
    external_module: AstElement<AstTypeReference> | null;
    value: AstElement<AstValueReference>;
}

export interface AstDefinedType {
    ast: 'DefinedType';
    external_module: AstElement<AstDefinedTypeModule> | null;
    ty: AstElement<AstTypeReference>;
}

export interface AstDefinedTypeModule {
    ast: 'DefinedTypeModule';
    item: AstElement<AstTypeReference>;
}

export interface AstSymbolList {
    ast: 'SymbolList';
    item: AstElement<AstSymbol>[];
}

export type AstSymbol = { ast: 'Symbol'; } & ({
    variant: 'ParameterizedReference';
    value: AstElement<AstParameterizedReference>;
} | {
    variant: 'Reference';
    value: AstElement<AstReference>;
});

export interface AstParameterizedReference {
    ast: 'ParameterizedReference';
    item: AstElement<AstReference>;
}

export type AstReference = { ast: 'Reference'; } & ({
    variant: 'TypeReference';
    value: AstElement<AstTypeReference>;
} | {
    variant: 'ValueReference';
    value: AstElement<AstValueReference>;
});

export type AstAssignment = { ast: 'Assignment'; } & ({
    variant: 'ObjectSetAssignment';
    value: AstElement<AstObjectSetAssignment>;
} | {
    variant: 'TypeAssignment';
    value: AstElement<AstTypeAssignment>;
} | {
    variant: 'ValueAssignment';
    value: AstElement<AstValueAssignment>;
});

export interface AstParameterDeclList {
    ast: 'ParameterDeclList';
    item: AstElement<AstParameterDecl>[];
}

export interface AstObjectSetParameterDecl {
    ast: 'ObjectSetParameterDecl';
    class_type: AstElement<AstUppercaseReference>;
    name: AstElement<AstTypeReference>;
}

export type AstParameterDecl = { ast: 'ParameterDecl'; } & ({
    variant: 'ObjectSetParameterDecl';
    value: AstElement<AstObjectSetParameterDecl>;
} | {
    variant: 'ValueParameterDecl';
    value: AstElement<AstValueParameterDecl>;
} | {
    variant: 'TypeParameterDecl';
    value: AstElement<AstTypeParameterDecl>;
});

export interface AstValueParameterDecl {
    ast: 'ValueParameterDecl';
    ty: AstElement<AstType>;
    name: AstElement<AstValueReference>;
}

export interface AstTypeParameterDecl {
    ast: 'TypeParameterDecl';
    item: AstElement<AstTypeReference>;
}

export interface AstTypeAssignment {
    ast: 'TypeAssignment';
    name: AstElement<AstTypeReference>;
    parameters: AstElement<AstParameterDeclList> | null;
    subject: AstElement<AstTypeAssignmentSubject>;
}

export interface AstTypeIdentifier {
    ast: 'TypeIdentifier';
}

export interface AstAbstractSyntax {
    ast: 'AbstractSyntax';
}

export type AstTypeAssignmentSubject = { ast: 'TypeAssignmentSubject'; } & ({
    variant: 'TypeIdentifier';
    value: AstElement<AstTypeIdentifier>;
} | {
    variant: 'AbstractSyntax';
    value: AstElement<AstAbstractSyntax>;
} | {
    variant: 'InformationObjectClass';
    value: AstElement<AstInformationObjectClass>;
} | {
    variant: 'Type';
    value: AstElement<AstType>;
});

export interface AstConstraint {
    ast: 'Constraint';
    item: AstElement<AstSubtypeConstraint>;
}

export interface AstSubtypeConstraint {
    ast: 'SubtypeConstraint';
    element_sets: AstElement<AstConstraintElement>[];
}

export type AstConstraintElement = { ast: 'ConstraintElement'; } & ({
    variant: 'Extensible';
    value: AstElement<AstExtensible>;
} | {
    variant: 'SubtypeElementSet';
    value: AstElement<AstSubtypeElementSet>;
});

export interface AstUnion {
    ast: 'Union';
}

export interface AstKeywordUnion {
    ast: 'KeywordUnion';
}

export interface AstIntersection {
    ast: 'Intersection';
}

export interface AstKeywordIntersection {
    ast: 'KeywordIntersection';
}

export type AstConstraintOperator = { ast: 'ConstraintOperator'; } & ({
    variant: 'Union';
    value: AstElement<AstUnion>;
} | {
    variant: 'KeywordUnion';
    value: AstElement<AstKeywordUnion>;
} | {
    variant: 'Intersection';
    value: AstElement<AstIntersection>;
} | {
    variant: 'KeywordIntersection';
    value: AstElement<AstKeywordIntersection>;
});

export interface AstSubtypeElementSet {
    ast: 'SubtypeElementSet';
    elements: AstElement<AstSubtypeElement>[];
    operators: AstElement<AstConstraintOperator>[];
}

export interface AstSubtypeElementGroup {
    ast: 'SubtypeElementGroup';
    item: AstElement<AstSubtypeElementSet>;
}

export interface AstValueRangeConstraint {
    ast: 'ValueRangeConstraint';
    lower: AstElement<AstRangeLowerBound>;
    upper: AstElement<AstRangeUpperBound>;
}

export interface AstSingleValueConstraint {
    ast: 'SingleValueConstraint';
    item: AstElement<AstValue>;
}

export interface AstContainedSubtype {
    ast: 'ContainedSubtype';
    item: AstElement<AstType>;
}

export type AstSubtypeElement = { ast: 'SubtypeElement'; } & ({
    variant: 'SubtypeElementGroup';
    value: AstElement<AstSubtypeElementGroup>;
} | {
    variant: 'UserDefinedConstraint';
    value: AstElement<AstUserDefinedConstraint>;
} | {
    variant: 'PatternConstraint';
    value: AstElement<AstPatternConstraint>;
} | {
    variant: 'ContentsConstraint';
    value: AstElement<AstContentsConstraint>;
} | {
    variant: 'InnerTypeConstraints';
    value: AstElement<AstInnerTypeConstraints>;
} | {
    variant: 'SizeConstraint';
    value: AstElement<AstSizeConstraint>;
} | {
    variant: 'TableConstraint';
    value: AstElement<AstTableConstraint>;
} | {
    variant: 'ValueRangeConstraint';
    value: AstElement<AstValueRangeConstraint>;
} | {
    variant: 'SingleValueConstraint';
    value: AstElement<AstSingleValueConstraint>;
} | {
    variant: 'ContainedSubtype';
    value: AstElement<AstContainedSubtype>;
});

export interface AstPatternConstraint {
    ast: 'PatternConstraint';
    item: AstElement<AstStringLiteral>;
}

export interface AstTableConstraint {
    ast: 'TableConstraint';
    set_name: AstElement<AstDefinedType>;
    component_ref: AstElement<AstComponentReference> | null;
}

export interface AstComponentReference {
    ast: 'ComponentReference';
    relative: boolean;
    elements: AstElement<AstValueReference>[];
}

export interface AstEncodedBy {
    ast: 'EncodedBy';
    ty: AstElement<AstType>;
    value: AstElement<AstValue>;
}

export interface AstContaining {
    ast: 'Containing';
    item: AstElement<AstType>;
}

export type AstContentsConstraint = { ast: 'ContentsConstraint'; } & ({
    variant: 'EncodedBy';
    value: AstElement<AstEncodedBy>;
} | {
    variant: 'Containing';
    value: AstElement<AstContaining>;
});

export interface AstUserDefinedConstraint {
    ast: 'UserDefinedConstraint';
    parameters: AstElement<AstUserDefinedConstraintParameter>[];
}

export type AstUserDefinedConstraintParameter = { ast: 'UserDefinedConstraintParameter'; } & ({
    variant: 'ValueParameterDecl';
    value: AstElement<AstValueParameterDecl>;
} | {
    variant: 'Type';
    value: AstElement<AstType>;
});

export interface AstSizeConstraint {
    ast: 'SizeConstraint';
    item: AstElement<AstConstraint>;
}

export interface AstGtValue {
    ast: 'GtValue';
    item: AstElement<AstRangeLowerBoundValue>;
}

export type AstRangeLowerBound = { ast: 'RangeLowerBound'; } & ({
    variant: 'GtValue';
    value: AstElement<AstGtValue>;
} | {
    variant: 'RangeLowerBoundValue';
    value: AstElement<AstRangeLowerBoundValue>;
});

export interface AstMin {
    ast: 'Min';
}

export type AstRangeLowerBoundValue = { ast: 'RangeLowerBoundValue'; } & ({
    variant: 'Min';
    value: AstElement<AstMin>;
} | {
    variant: 'Value';
    value: AstElement<AstValue>;
});

export interface AstLtValue {
    ast: 'LtValue';
    item: AstElement<AstRangeUpperBoundValue>;
}

export type AstRangeUpperBound = { ast: 'RangeUpperBound'; } & ({
    variant: 'LtValue';
    value: AstElement<AstLtValue>;
} | {
    variant: 'RangeUpperBoundValue';
    value: AstElement<AstRangeUpperBoundValue>;
});

export interface AstMax {
    ast: 'Max';
}

export type AstRangeUpperBoundValue = { ast: 'RangeUpperBoundValue'; } & ({
    variant: 'Max';
    value: AstElement<AstMax>;
} | {
    variant: 'Value';
    value: AstElement<AstValue>;
});

export type AstInnerTypeConstraints = { ast: 'InnerTypeConstraints'; } & ({
    variant: 'SingleTypeConstraint';
    value: AstElement<AstSingleTypeConstraint>;
} | {
    variant: 'MultipleTypeConstraints';
    value: AstElement<AstMultipleTypeConstraints>;
});

export interface AstSingleTypeConstraint {
    ast: 'SingleTypeConstraint';
    item: AstElement<AstConstraint>;
}

export interface AstMultipleTypeConstraints {
    ast: 'MultipleTypeConstraints';
    item: AstElement<AstTypeConstraintSpec>;
}

export interface AstFullSpec {
    ast: 'FullSpec';
    item: AstElement<AstTypeConstraints>;
}

export interface AstPartialSpec {
    ast: 'PartialSpec';
    item: AstElement<AstTypeConstraints>;
}

export type AstTypeConstraintSpec = { ast: 'TypeConstraintSpec'; } & ({
    variant: 'FullSpec';
    value: AstElement<AstFullSpec>;
} | {
    variant: 'PartialSpec';
    value: AstElement<AstPartialSpec>;
});

export interface AstTypeConstraints {
    ast: 'TypeConstraints';
    item: AstElement<AstNamedConstraint>[];
}

export interface AstNamedConstraint {
    ast: 'NamedConstraint';
    name: AstElement<AstValueReference>;
    constraint: AstElement<AstComponentConstraint>;
}

export interface AstComponentConstraint {
    ast: 'ComponentConstraint';
    value: AstElement<AstConstraint> | null;
    presence: AstElement<AstPresenceConstraint> | null;
}

export interface AstPresencePresent {
    ast: 'PresencePresent';
}

export interface AstPresenceAbsent {
    ast: 'PresenceAbsent';
}

export interface AstPresenceOptional {
    ast: 'PresenceOptional';
}

export type AstPresenceConstraint = { ast: 'PresenceConstraint'; } & ({
    variant: 'PresencePresent';
    value: AstElement<AstPresencePresent>;
} | {
    variant: 'PresenceAbsent';
    value: AstElement<AstPresenceAbsent>;
} | {
    variant: 'PresenceOptional';
    value: AstElement<AstPresenceOptional>;
});

export interface AstAny {
    ast: 'Any';
    defined_by: AstElement<AstValue> | null;
}

export interface AstBoolean {
    ast: 'Boolean';
}

export interface AstInteger {
    ast: 'Integer';
    named_values: AstElement<AstNamedNumberList> | null;
}

export interface AstBitString {
    ast: 'BitString';
    named_bits: AstElement<AstNamedNumberList> | null;
}

export interface AstOctetString {
    ast: 'OctetString';
}

export interface AstNull {
    ast: 'Null';
}

export interface AstObjectIdentifier {
    ast: 'ObjectIdentifier';
}

export interface AstObjectDescriptor {
    ast: 'ObjectDescriptor';
}

export interface AstExternal {
    ast: 'External';
}

export interface AstReal {
    ast: 'Real';
}

export interface AstNamedEnumerationItem {
    ast: 'NamedEnumerationItem';
    name: AstElement<AstValueReference>;
    num: AstElement<AstEnumerationItemNum> | null;
}

export type AstEnumerationItem = { ast: 'EnumerationItem'; } & ({
    variant: 'Extensible';
    value: AstElement<AstExtensible>;
} | {
    variant: 'NamedEnumerationItem';
    value: AstElement<AstNamedEnumerationItem>;
});

export interface AstEnumerationItemNum {
    ast: 'EnumerationItemNum';
    item: AstElement<AstValue>;
}

export interface AstEnumerated {
    ast: 'Enumerated';
    item: AstElement<AstEnumerationItem>[];
}

export interface AstEmbeddedPDV {
    ast: 'EmbeddedPDV';
}

export interface AstUTF8String {
    ast: 'UTF8String';
}

export interface AstRelativeOid {
    ast: 'RelativeOid';
}

export interface AstTime {
    ast: 'Time';
}

export interface AstSequence {
    ast: 'Sequence';
}

export interface AstSet {
    ast: 'Set';
}

export type AstStructureKind = { ast: 'StructureKind'; } & ({
    variant: 'Sequence';
    value: AstElement<AstSequence>;
} | {
    variant: 'Set';
    value: AstElement<AstSet>;
});

export interface AstStructure {
    ast: 'Structure';
    kind: AstElement<AstStructureKind>;
    component_groups: AstElement<AstStructureComponentGroup>[];
}

export type AstConstraintOrSizeConstraint = { ast: 'ConstraintOrSizeConstraint'; } & ({
    variant: 'Constraint';
    value: AstElement<AstConstraint>;
} | {
    variant: 'SizeConstraint';
    value: AstElement<AstSizeConstraint>;
});

export interface AstStructureOf {
    ast: 'StructureOf';
    kind: AstElement<AstStructureKind>;
    constraint: AstElement<AstConstraintOrSizeConstraint> | null;
    ty: AstElement<AstType>;
}

export interface AstExtensible {
    ast: 'Extensible';
}

export interface AstComponentExtensionGroup {
    ast: 'ComponentExtensionGroup';
    version: AstElement<AstNumber> | null;
    components: AstElement<AstStructureComponent>[];
}

export type AstStructureComponentGroup = { ast: 'StructureComponentGroup'; } & ({
    variant: 'Extensible';
    value: AstElement<AstExtensible>;
} | {
    variant: 'ComponentExtensionGroup';
    value: AstElement<AstComponentExtensionGroup>;
} | {
    variant: 'StructureComponent';
    value: AstElement<AstStructureComponent>;
});

export interface AstComponentsOf {
    ast: 'ComponentsOf';
    item: AstElement<AstType>;
}

export interface AstNamedStructureComponent {
    ast: 'NamedStructureComponent';
    name: AstElement<AstValueReference>;
    ty: AstElement<AstType>;
    default: AstElement<AstValue> | null;
    optional: boolean;
}

export type AstStructureComponent = { ast: 'StructureComponent'; } & ({
    variant: 'ComponentsOf';
    value: AstElement<AstComponentsOf>;
} | {
    variant: 'NamedStructureComponent';
    value: AstElement<AstNamedStructureComponent>;
});

export interface AstObjectClassFieldType {
    ast: 'ObjectClassFieldType';
    class_type: AstElement<AstUppercaseReference>;
    field: AstElement<AstFieldReference>;
}

export interface AstChoice {
    ast: 'Choice';
    item: AstElement<AstChoiceAlternative>[];
}

export interface AstNamedChoiceAlternative {
    ast: 'NamedChoiceAlternative';
    name: AstElement<AstValueReference>;
    ty: AstElement<AstType>;
}

export type AstChoiceAlternative = { ast: 'ChoiceAlternative'; } & ({
    variant: 'Extensible';
    value: AstElement<AstExtensible>;
} | {
    variant: 'NamedChoiceAlternative';
    value: AstElement<AstNamedChoiceAlternative>;
});

export interface AstNumericString {
    ast: 'NumericString';
}

export interface AstPrintableString {
    ast: 'PrintableString';
}

export interface AstTeletexString {
    ast: 'TeletexString';
}

export interface AstVideotexString {
    ast: 'VideotexString';
}

export interface AstIA5String {
    ast: 'IA5String';
}

export interface AstUTCTime {
    ast: 'UTCTime';
}

export interface AstGeneralizedTime {
    ast: 'GeneralizedTime';
}

export interface AstGraphicString {
    ast: 'GraphicString';
}

export interface AstVisibleString {
    ast: 'VisibleString';
}

export interface AstGeneralString {
    ast: 'GeneralString';
}

export interface AstUniversalString {
    ast: 'UniversalString';
}

export interface AstCharacterString {
    ast: 'CharacterString';
}

export interface AstBMPString {
    ast: 'BMPString';
}

export interface AstDate {
    ast: 'Date';
}

export interface AstTimeOfDay {
    ast: 'TimeOfDay';
}

export interface AstDateTime {
    ast: 'DateTime';
}

export interface AstDuration {
    ast: 'Duration';
}

export interface AstInstanceOf {
    ast: 'InstanceOf';
    class_type: AstElement<AstUppercaseReference>;
}

export type AstBuiltinType = { ast: 'BuiltinType'; } & ({
    variant: 'Any';
    value: AstElement<AstAny>;
} | {
    variant: 'Boolean';
    value: AstElement<AstBoolean>;
} | {
    variant: 'Integer';
    value: AstElement<AstInteger>;
} | {
    variant: 'BitString';
    value: AstElement<AstBitString>;
} | {
    variant: 'OctetString';
    value: AstElement<AstOctetString>;
} | {
    variant: 'Null';
    value: AstElement<AstNull>;
} | {
    variant: 'ObjectIdentifier';
    value: AstElement<AstObjectIdentifier>;
} | {
    variant: 'ObjectDescriptor';
    value: AstElement<AstObjectDescriptor>;
} | {
    variant: 'External';
    value: AstElement<AstExternal>;
} | {
    variant: 'Real';
    value: AstElement<AstReal>;
} | {
    variant: 'Enumerated';
    value: AstElement<AstEnumerated>;
} | {
    variant: 'EmbeddedPDV';
    value: AstElement<AstEmbeddedPDV>;
} | {
    variant: 'UTF8String';
    value: AstElement<AstUTF8String>;
} | {
    variant: 'RelativeOid';
    value: AstElement<AstRelativeOid>;
} | {
    variant: 'Time';
    value: AstElement<AstTime>;
} | {
    variant: 'Structure';
    value: AstElement<AstStructure>;
} | {
    variant: 'Choice';
    value: AstElement<AstChoice>;
} | {
    variant: 'NumericString';
    value: AstElement<AstNumericString>;
} | {
    variant: 'PrintableString';
    value: AstElement<AstPrintableString>;
} | {
    variant: 'TeletexString';
    value: AstElement<AstTeletexString>;
} | {
    variant: 'VideotexString';
    value: AstElement<AstVideotexString>;
} | {
    variant: 'IA5String';
    value: AstElement<AstIA5String>;
} | {
    variant: 'UTCTime';
    value: AstElement<AstUTCTime>;
} | {
    variant: 'GeneralizedTime';
    value: AstElement<AstGeneralizedTime>;
} | {
    variant: 'GraphicString';
    value: AstElement<AstGraphicString>;
} | {
    variant: 'VisibleString';
    value: AstElement<AstVisibleString>;
} | {
    variant: 'GeneralString';
    value: AstElement<AstGeneralString>;
} | {
    variant: 'UniversalString';
    value: AstElement<AstUniversalString>;
} | {
    variant: 'CharacterString';
    value: AstElement<AstCharacterString>;
} | {
    variant: 'BMPString';
    value: AstElement<AstBMPString>;
} | {
    variant: 'Date';
    value: AstElement<AstDate>;
} | {
    variant: 'TimeOfDay';
    value: AstElement<AstTimeOfDay>;
} | {
    variant: 'DateTime';
    value: AstElement<AstDateTime>;
} | {
    variant: 'Duration';
    value: AstElement<AstDuration>;
} | {
    variant: 'InstanceOf';
    value: AstElement<AstInstanceOf>;
});

export interface AstUniversal {
    ast: 'Universal';
}

export interface AstApplication {
    ast: 'Application';
}

export interface AstPrivate {
    ast: 'Private';
}

export type AstClass = { ast: 'Class'; } & ({
    variant: 'Universal';
    value: AstElement<AstUniversal>;
} | {
    variant: 'Application';
    value: AstElement<AstApplication>;
} | {
    variant: 'Private';
    value: AstElement<AstPrivate>;
});

export interface AstTag {
    ast: 'Tag';
    encoding_reference: AstElement<AstUppercaseReference> | null;
    class: AstElement<AstClass> | null;
    class_number: AstElement<AstNumber>;
}

export interface AstTagKindImplicit {
    ast: 'TagKindImplicit';
}

export interface AstTagKindExplicit {
    ast: 'TagKindExplicit';
}

export type AstTagKind = { ast: 'TagKind'; } & ({
    variant: 'TagKindImplicit';
    value: AstElement<AstTagKindImplicit>;
} | {
    variant: 'TagKindExplicit';
    value: AstElement<AstTagKindExplicit>;
});

export interface AstTaggedType {
    ast: 'TaggedType';
    tag: AstElement<AstTag>;
    kind: AstElement<AstTagKind> | null;
    ty: AstElement<AstType>;
}

export interface AstSuffixed {
    ast: 'Suffixed';
    ty: AstElement<AstUntaggedType>;
    constraints: AstElement<AstConstraint>[];
}

export interface AstTypeWithConstraint {
    ast: 'TypeWithConstraint';
    item: AstElement<AstStructureOf>;
}

export type AstConstrainedType = { ast: 'ConstrainedType'; } & ({
    variant: 'Suffixed';
    value: AstElement<AstSuffixed>;
} | {
    variant: 'TypeWithConstraint';
    value: AstElement<AstTypeWithConstraint>;
});

export interface AstParameterizedDefinedType {
    ast: 'ParameterizedDefinedType';
    name: AstElement<AstDefinedType>;
    parameters: AstElement<AstParameterList>;
}

export interface AstParameterList {
    ast: 'ParameterList';
    item: AstElement<AstParameter>[];
}

export interface AstObjectSetParameter {
    ast: 'ObjectSetParameter';
    item: AstElement<AstDefinedType>;
}

export interface AstTypeParameter {
    ast: 'TypeParameter';
    item: AstElement<AstType>;
}

export interface AstValueParameter {
    ast: 'ValueParameter';
    item: AstElement<AstValue>;
}

export type AstParameter = { ast: 'Parameter'; } & ({
    variant: 'ObjectSetParameter';
    value: AstElement<AstObjectSetParameter>;
} | {
    variant: 'TypeParameter';
    value: AstElement<AstTypeParameter>;
} | {
    variant: 'ValueParameter';
    value: AstElement<AstValueParameter>;
});

export type AstUntaggedType = { ast: 'UntaggedType'; } & ({
    variant: 'BuiltinType';
    value: AstElement<AstBuiltinType>;
} | {
    variant: 'ObjectClassFieldType';
    value: AstElement<AstObjectClassFieldType>;
} | {
    variant: 'ParameterizedDefinedType';
    value: AstElement<AstParameterizedDefinedType>;
} | {
    variant: 'DefinedType';
    value: AstElement<AstDefinedType>;
});

export type AstType = { ast: 'Type'; } & ({
    variant: 'TaggedType';
    value: AstElement<AstTaggedType>;
} | {
    variant: 'ConstrainedType';
    value: AstElement<AstConstrainedType>;
});

export interface AstValueAssignment {
    ast: 'ValueAssignment';
    name: AstElement<AstValueReference>;
    ty: AstElement<AstType>;
    value: AstElement<AstValue>;
}

export interface AstObjectSetAssignment {
    ast: 'ObjectSetAssignment';
    name: AstElement<AstTypeReference>;
    ty: AstElement<AstUppercaseReference>;
    subject: AstElement<AstObjectSetAssignmentSubject>;
}

export interface AstEmptyExtensible {
    ast: 'EmptyExtensible';
}

export interface AstObjectSet {
    ast: 'ObjectSet';
    element_groups: AstElement<AstObjectSetElementGroup>[];
}

export type AstObjectSetAssignmentSubject = { ast: 'ObjectSetAssignmentSubject'; } & ({
    variant: 'EmptyExtensible';
    value: AstElement<AstEmptyExtensible>;
} | {
    variant: 'ObjectSet';
    value: AstElement<AstObjectSet>;
});

export interface AstObjectSetElements {
    ast: 'ObjectSetElements';
    item: AstElement<AstObjectSetElement>[];
}

export type AstObjectSetElementGroup = { ast: 'ObjectSetElementGroup'; } & ({
    variant: 'Extensible';
    value: AstElement<AstExtensible>;
} | {
    variant: 'ObjectSetElements';
    value: AstElement<AstObjectSetElements>;
});

export type AstObjectSetElement = { ast: 'ObjectSetElement'; } & ({
    variant: 'ObjectFieldReference';
    value: AstElement<AstObjectFieldReference>;
} | {
    variant: 'Object';
    value: AstElement<AstObject>;
} | {
    variant: 'DefinedType';
    value: AstElement<AstDefinedType>;
});

export interface AstValueSet {
    ast: 'ValueSet';
    item: AstElement<AstValueSetElementGroup>[];
}

export interface AstValueSetElements {
    ast: 'ValueSetElements';
    item: AstElement<AstValue>[];
}

export type AstValueSetElementGroup = { ast: 'ValueSetElementGroup'; } & ({
    variant: 'Extensible';
    value: AstElement<AstExtensible>;
} | {
    variant: 'ValueSetElements';
    value: AstElement<AstValueSetElements>;
});

export interface AstNamedBitStringValue {
    ast: 'NamedBitStringValue';
    item: AstElement<AstValueReference>[];
}

export interface AstStructureValue {
    ast: 'StructureValue';
    components: AstElement<AstStructureValueComponent>[];
}

export interface AstStructureValueComponent {
    ast: 'StructureValueComponent';
    name: AstElement<AstValueReference>;
    value: AstElement<AstValue>;
}

export interface AstStructureOfValue {
    ast: 'StructureOfValue';
    item: AstElement<AstValue>[];
}

export interface AstChoiceValue {
    ast: 'ChoiceValue';
    alternative: AstElement<AstValueReference>;
    value: AstElement<AstValue>;
}

export interface AstOpenTypeValue {
    ast: 'OpenTypeValue';
    open_type: AstElement<AstOpenTypeValueTypeReference>;
    value: AstElement<AstValue>;
}

export type AstOpenTypeValueTypeReference = { ast: 'OpenTypeValueTypeReference'; } & ({
    variant: 'ObjectFieldReference';
    value: AstElement<AstObjectFieldReference>;
} | {
    variant: 'Type';
    value: AstElement<AstType>;
});

export interface AstNegative {
    ast: 'Negative';
}

export interface AstIntegerValue {
    ast: 'IntegerValue';
    sign: AstElement<AstNegative> | null;
    value: AstElement<AstNumber>;
}

export interface AstDecimalValue {
    ast: 'DecimalValue';
    sign: AstElement<AstNegative> | null;
    whole: AstElement<AstNumber>;
    fraction: AstElement<AstNumber>;
}

export interface AstTrue {
    ast: 'True';
}

export interface AstFalse {
    ast: 'False';
}

export type AstBooleanValue = { ast: 'BooleanValue'; } & ({
    variant: 'True';
    value: AstElement<AstTrue>;
} | {
    variant: 'False';
    value: AstElement<AstFalse>;
});

export interface AstPlusInfinity {
    ast: 'PlusInfinity';
}

export interface AstMinusInfinity {
    ast: 'MinusInfinity';
}

export interface AstNotANumber {
    ast: 'NotANumber';
}

export type AstSpecialRealValue = { ast: 'SpecialRealValue'; } & ({
    variant: 'PlusInfinity';
    value: AstElement<AstPlusInfinity>;
} | {
    variant: 'MinusInfinity';
    value: AstElement<AstMinusInfinity>;
} | {
    variant: 'NotANumber';
    value: AstElement<AstNotANumber>;
});

export interface AstContainingValue {
    ast: 'ContainingValue';
    item: AstElement<AstValue>;
}

export type AstBuiltinValue = { ast: 'BuiltinValue'; } & ({
    variant: 'ContainingValue';
    value: AstElement<AstContainingValue>;
} | {
    variant: 'OpenTypeValue';
    value: AstElement<AstOpenTypeValue>;
} | {
    variant: 'ObjectFieldReference';
    value: AstElement<AstObjectFieldReference>;
} | {
    variant: 'Null';
    value: AstElement<AstNull>;
} | {
    variant: 'BooleanValue';
    value: AstElement<AstBooleanValue>;
} | {
    variant: 'SpecialRealValue';
    value: AstElement<AstSpecialRealValue>;
} | {
    variant: 'StringLiteral';
    value: AstElement<AstStringLiteral>;
} | {
    variant: 'DecimalValue';
    value: AstElement<AstDecimalValue>;
} | {
    variant: 'IntegerValue';
    value: AstElement<AstIntegerValue>;
} | {
    variant: 'ChoiceValue';
    value: AstElement<AstChoiceValue>;
} | {
    variant: 'BracedTokenStream';
    value: AstElement<AstBracedTokenStream>;
});

export type AstValue = { ast: 'Value'; } & ({
    variant: 'BuiltinValue';
    value: AstElement<AstBuiltinValue>;
} | {
    variant: 'DefinedValue';
    value: AstElement<AstDefinedValue>;
});

export type AstIntegerValueReference = { ast: 'IntegerValueReference'; } & ({
    variant: 'IntegerValue';
    value: AstElement<AstIntegerValue>;
} | {
    variant: 'DefinedValue';
    value: AstElement<AstDefinedValue>;
});

export interface AstInformationObjectClass {
    ast: 'InformationObjectClass';
    fields: AstElement<AstObjectClassField>[];
    syntax: AstElement<AstSyntaxList>;
}

export type AstObjectClassField = { ast: 'ObjectClassField'; } & ({
    variant: 'ValueField';
    value: AstElement<AstValueField>;
} | {
    variant: 'SetField';
    value: AstElement<AstSetField>;
} | {
    variant: 'TypeField';
    value: AstElement<AstTypeField>;
});

export interface AstTypeField {
    ast: 'TypeField';
    name: AstElement<AstTypeFieldReference>;
    optional: boolean;
}

export interface AstSetField {
    ast: 'SetField';
    name: AstElement<AstTypeFieldReference>;
    element_type: AstElement<AstType>;
    default: AstElement<AstBracedTokenStream> | null;
    optional: boolean;
}

export interface AstValueField {
    ast: 'ValueField';
    name: AstElement<AstValueFieldReference>;
    subject: AstElement<AstValueFieldSubject>;
    default: AstElement<AstValue> | null;
    optional: boolean;
    unique: boolean;
}

export interface AstOpenTypeReference {
    ast: 'OpenTypeReference';
    item: AstElement<AstTypeFieldReference>;
}

export type AstValueFieldSubject = { ast: 'ValueFieldSubject'; } & ({
    variant: 'OpenTypeReference';
    value: AstElement<AstOpenTypeReference>;
} | {
    variant: 'Type';
    value: AstElement<AstType>;
});

export interface AstTypeFieldReference {
    ast: 'TypeFieldReference';
    item: AstElement<AstTypeReference>;
}

export interface AstValueFieldReference {
    ast: 'ValueFieldReference';
    item: AstElement<AstValueReference>;
}

export type AstFieldReference = { ast: 'FieldReference'; } & ({
    variant: 'TypeFieldReference';
    value: AstElement<AstTypeFieldReference>;
} | {
    variant: 'ValueFieldReference';
    value: AstElement<AstValueFieldReference>;
});

export interface AstSyntaxList {
    ast: 'SyntaxList';
    elements: AstElement<AstSyntaxListElement>[];
}

export interface AstOptionalSyntaxElement {
    ast: 'OptionalSyntaxElement';
    item: AstElement<AstSyntaxListElement>[];
}

export interface AstRequiredSyntaxElement {
    ast: 'RequiredSyntaxElement';
    item: AstElement<AstSyntaxItem>;
}

export type AstSyntaxListElement = { ast: 'SyntaxListElement'; } & ({
    variant: 'OptionalSyntaxElement';
    value: AstElement<AstOptionalSyntaxElement>;
} | {
    variant: 'RequiredSyntaxElement';
    value: AstElement<AstRequiredSyntaxElement>;
});

export type AstSyntaxItem = { ast: 'SyntaxItem'; } & ({
    variant: 'FieldReference';
    value: AstElement<AstFieldReference>;
} | {
    variant: 'SyntaxTokenLiteral';
    value: AstElement<AstSyntaxTokenLiteral>;
});

export interface AstObjectFieldReference {
    ast: 'ObjectFieldReference';
    object_name: AstElement<AstDefinedValue>;
    field_ref: AstElement<AstFieldReference>;
}

export type AstObject = { ast: 'Object'; } & ({
    variant: 'DefinedValue';
    value: AstElement<AstDefinedValue>;
} | {
    variant: 'BracedTokenStream';
    value: AstElement<AstBracedTokenStream>;
});

export type AstItem = Soi | Eoi | AstTypeReference | AstUppercaseReference | AstValueReference | AstStringLiteral | AstNumber | Keyword | Operator | AstBracedTokenStream | AstSyntaxTokenLiteral | AstProgram | AstModule | AstModuleHeader | AstModuleBody | AstModuleIdentifier | AstDefinitiveOidWithIri | AstDefinitiveOid | AstDefinitiveOidComponent | AstNamedNumber | AstNamedNumberList | AstTagDefault | AstTagDefaultKind | AstExtensibilityImplied | AstExports | AstExportsKind | AstImports | AstSymbolsFromModule | AstSelectionOption | AstSelectionOptionKind | AstAssignedIdentifier | AstGlobalModuleReference | AstOid | AstObjectIdentifierValue | AstObjectIdentifierComponent | AstDefinedValue | AstDefinedType | AstDefinedTypeModule | AstSymbolList | AstSymbol | AstParameterizedReference | AstReference | AstAssignment | AstParameterDeclList | AstParameterDecl | AstValueParameterDecl | AstTypeParameterDecl | AstTypeAssignment | AstTypeAssignmentSubject | AstConstraint | AstSubtypeConstraint | AstConstraintElement | AstConstraintOperator | AstSubtypeElementSet | AstSubtypeElement | AstPatternConstraint | AstTableConstraint | AstComponentReference | AstContentsConstraint | AstUserDefinedConstraint | AstUserDefinedConstraintParameter | AstSizeConstraint | AstRangeLowerBound | AstRangeLowerBoundValue | AstRangeUpperBound | AstRangeUpperBoundValue | AstInnerTypeConstraints | AstSingleTypeConstraint | AstMultipleTypeConstraints | AstTypeConstraintSpec | AstTypeConstraints | AstNamedConstraint | AstComponentConstraint | AstPresenceConstraint | AstAny | AstBoolean | AstInteger | AstBitString | AstOctetString | AstNull | AstObjectIdentifier | AstObjectDescriptor | AstExternal | AstReal | AstEnumerationItem | AstEnumerationItemNum | AstEnumerated | AstEmbeddedPDV | AstUTF8String | AstRelativeOid | AstTime | AstStructureKind | AstStructure | AstConstraintOrSizeConstraint | AstStructureOf | AstExtensible | AstStructureComponentGroup | AstStructureComponent | AstObjectClassFieldType | AstChoice | AstChoiceAlternative | AstNumericString | AstPrintableString | AstTeletexString | AstVideotexString | AstIA5String | AstUTCTime | AstGeneralizedTime | AstGraphicString | AstVisibleString | AstGeneralString | AstUniversalString | AstCharacterString | AstBMPString | AstDate | AstTimeOfDay | AstDateTime | AstDuration | AstInstanceOf | AstBuiltinType | AstClass | AstTag | AstTagKind | AstTaggedType | AstConstrainedType | AstParameterizedDefinedType | AstParameterList | AstParameter | AstUntaggedType | AstType | AstValueAssignment | AstObjectSetAssignment | AstObjectSetAssignmentSubject | AstObjectSetElementGroup | AstObjectSetElement | AstValueSet | AstValueSetElementGroup | AstNamedBitStringValue | AstStructureValue | AstStructureValueComponent | AstStructureOfValue | AstChoiceValue | AstOpenTypeValue | AstOpenTypeValueTypeReference | AstNegative | AstIntegerValue | AstDecimalValue | AstBooleanValue | AstSpecialRealValue | AstContainingValue | AstBuiltinValue | AstValue | AstIntegerValueReference | AstInformationObjectClass | AstObjectClassField | AstTypeField | AstSetField | AstValueField | AstValueFieldSubject | AstTypeFieldReference | AstValueFieldReference | AstFieldReference | AstSyntaxList | AstSyntaxListElement | AstSyntaxItem | AstObjectFieldReference | AstObject;
