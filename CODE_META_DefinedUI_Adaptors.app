'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META_DefinedUI createSubApplication: #CODE_META_DefinedUI_Adaptors in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_DefinedUI_Adaptors becomeDefault!

METAAspectAdaptorWithCheck subclass: #CMDFMetaInfoAspectAdaptor
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Adaptors becomeDefault!

METAAspectAdaptorWithCheck subclass: #CMDFValueExpressionAspectAdaptor
	instanceVariableNames: 'valueExpression '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Adaptors becomeDefault!

PrintConverter subclass: #CMPrintConverter
	instanceVariableNames: 'childSpec '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Adaptors becomeDefault!

SubApplication subclass: #CODE_META_DefinedUI_Adaptors
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Adaptors becomeDefault!

!CMDFMetaInfoAspectAdaptor class publicMethodsFor: 'instance creation'!

newWithMetaInfo: theMetaInfo
	| anAdaptor |
	anAdaptor := self new.
	anAdaptor metaInfo: theMetaInfo.
	^anAdaptor! !

!CMDFMetaInfoAspectAdaptor publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo!

setValue: theValue 

	| aValue anOldValue aNewValue aMetaInfo |

	self isReadOnly ifTrue: [ 
		Dialog warn: 'This is Read Only information'.
		^nil].
	anOldValue := self value.

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := self setValueUsingTarget: self target to: theValue.

	aNewValue := self value.
	(editor isNil not and: [ (anOldValue = aNewValue) not]) ifTrue: [
		editor itemHasChanged: getSelector.
		editor recordItemHasChanged: (Array with: getSelector with: anOldValue with: aNewValue)].

	^aValue!

setValueUsingTarget: theObject to: theValue
	"Set the value of anObject by sending the receiver's 
	store (put) selector to the anObject with argument newValue."

	| aMetaInfo aValue |
	theObject == nil ifTrue: [^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo object: theObject setTC: theValue.
	^aValue!

valueUsingTarget: theObject
	"Answer the value returned by sending the receiver's retrieval (get)  selector to anObject"

	| aMetaInfo aValue |
	theObject == nil ifTrue: [^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theObject.
	^aValue! !

!CMDFMetaInfoAspectAdaptor publicMethodsFor: 'updating'!

update: anAspect with: parameter from: sender

	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	(sender == subject and: [anAspect == aMetaInfo name asSymbol])
		ifTrue: [dependents update: #value with: parameter from: self]
		ifFalse: [super update: anAspect with: parameter from: sender]! !

!CMDFValueExpressionAspectAdaptor class publicMethodsFor: 'instance creation'!

newWithExpression: theExpression
	| anAdaptor |
	anAdaptor := self new.
	anAdaptor valueExpression: theExpression.
	^anAdaptor! !

!CMDFValueExpressionAspectAdaptor publicMethodsFor: 'accessing'!

setValue: theValue 

	| aValue anOldValue aNewValue |

	self isReadOnly ifTrue: [ 
		Dialog warn: 'This is Read Only information'.
		^nil].
	anOldValue := self value.
 
	aValue := self setValueUsingTarget: self target to: theValue.

	aNewValue := self value.
	(editor isNil not and: [ (anOldValue = aNewValue) not]) ifTrue: [
		editor itemHasChanged: getSelector.
		editor recordItemHasChanged: (Array with: getSelector with: anOldValue with: aNewValue)].

	^aValue!

setValueUsingTarget: theObject to: theValue
	"Set the value of anObject by sending the receiver's 
	store (put) selector to the anObject with argument newValue."

	| anObjectMetaInfo anExpression aValue |
	theObject isNil ifTrue: [^nil].

	anObjectMetaInfo := theObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [^nil].

	anExpression := self valueExpression.
	(anExpression isNil or: [ anExpression isEmpty]) ifTrue: [ ^nil].

	aValue := anObjectMetaInfo setObject: theObject expression: anExpression value: theValue.
	^aValue!

valueExpression
	^valueExpression!

valueExpression: theExpression
	valueExpression := theExpression!

valueUsingTarget: theObject
	"Answer the value returned by sending the receiver's retrieval (get)  selector to anObject"

	| aValue anObjectMetaInfo anExpression |

	theObject isNil ifTrue: [^nil].

	anObjectMetaInfo := theObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [^nil].

	anExpression := self valueExpression.
	(anExpression isNil or: [ anExpression isEmpty]) ifTrue: [ ^nil].

	aValue := anObjectMetaInfo getObject: theObject expression: anExpression.
	^aValue! !

!CMDFValueExpressionAspectAdaptor publicMethodsFor: 'updating'!

update: anAspect with: parameter from: sender

	| aValueExpression |

	aValueExpression := self valueExpression.
	(aValueExpression isNil or: [ aValueExpression isEmpty]) ifTrue: [ ^nil].

	(sender == subject and: [ (aValueExpression findString: anAspect asString startingAt: 1) > 0])
		ifTrue: [dependents update: #value with: parameter from: self]
		ifFalse: [super update: anAspect with: parameter from: sender]! !

!CMPrintConverter class publicMethodsFor: 'instance creation'!

forChildSpec: theChildSpec

	| aConverter |
	aConverter := self new.
	aConverter forChildSpec: theChildSpec.
	^aConverter! !

!CMPrintConverter publicMethodsFor: 'accessing'!

childSpec
	^childSpec!

toFormatBlock: theBlock	
	toFormat := theBlock!

toPrintBlock: theBlock	
	toPrint := theBlock!

toReadBlock: theBlock	
	toRead := theBlock! !

!CMPrintConverter publicMethodsFor: 'initalize-release'!

forChildSpec: theChildSpec
	childSpec := theChildSpec.! !

CMDFMetaInfoAspectAdaptor initializeAfterLoad!
CMDFValueExpressionAspectAdaptor initializeAfterLoad!
CMPrintConverter initializeAfterLoad!
CODE_META_DefinedUI_Adaptors initializeAfterLoad!

CODE_META_DefinedUI_Adaptors loaded!
