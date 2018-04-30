'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Adaptors in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Adaptors becomeDefault!

METAAspectAdaptorWithCheck subclass: #CMAspectAdaptorWithCheck
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Adaptors becomeDefault!

CMAspectAdaptorWithCheck subclass: #CMEnumAspectAdaptorWithCheck
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Adaptors becomeDefault!

CMAspectAdaptorWithCheck subclass: #CMOperationAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Adaptors becomeDefault!

METAClassAspectAdaptorWithCheck subclass: #CMClassAspectAdaptorWithCheck
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Adaptors becomeDefault!

METACodeAspectAdaptorWithCheck subclass: #CMCodeAspectAdaptorWithCheck
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Adaptors becomeDefault!

METATerminalAspectAdaptorWithCheck subclass: #CMTerminalAspectAdaptorWithCheck
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Adaptors becomeDefault!

SubApplication subclass: #CODE_META_Adaptors
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Adaptors becomeDefault!

!CMAspectAdaptorWithCheck class publicMethodsFor: 'instance creation'!

newWithMetaInfo: theMetaInfo
	| anAdaptor |
	anAdaptor := self new.
	anAdaptor metaInfo: theMetaInfo.
	^anAdaptor! !

!CMAspectAdaptorWithCheck publicMethodsFor: 'accessing'!

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

!CMAspectAdaptorWithCheck publicMethodsFor: 'updating'!

update: anAspect with: parameter from: sender

	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	(sender == subject and: [anAspect == aMetaInfo name asSymbol])
		ifTrue: [dependents update: #value with: parameter from: self]
		ifFalse: [super update: anAspect with: parameter from: sender]! !

!CMClassAspectAdaptorWithCheck class publicMethodsFor: 'instance creation'!

newWithMetaInfo: theMetaInfo
	| anAdaptor |
	anAdaptor := self new.
	anAdaptor metaInfo: theMetaInfo.
	^anAdaptor! !

!CMClassAspectAdaptorWithCheck publicMethodsFor: 'accessing'!

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

	aValue := aMetaInfo setObject: theObject featureValue: theValue.
	^aValue!

value
	"Answer the value returned by sending the receiver's retrieval (get) 
	selector to the receiver's subject."

	| aValue aMetaInfo aType |
	subject isNil ifTrue: [^'(none)' copy].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].
	
	aType := aMetaInfo referencedType.
	aType isNil ifTrue: [ ^nil].

	aValue := aType getObjectNameValue: subject.
		
	^aValue isNil 
		ifTrue: ['(none)' copy]
		ifFalse: [ aValue]!

valueUsingTarget: theObject
	"Answer the value returned by sending the receiver's retrieval (get)  selector to anObject"

	| aMetaInfo aValue |
	theObject == nil ifTrue: [^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theObject.
	^aValue! !

!CMCodeAspectAdaptorWithCheck class publicMethodsFor: 'instance creation'!

newWithMetaInfo: theMetaInfo
	| anAdaptor |
	anAdaptor := self new.
	anAdaptor metaInfo: theMetaInfo.
	^anAdaptor! !

!CMCodeAspectAdaptorWithCheck publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo!

setValue: theValue 

	[
		| aNewCodeOwner aSubject anEditor | 
		aSubject := subject.
		anEditor  := editor editorToTryToSetSelection.
		aNewCodeOwner := self setValueWithController: theValue.
		(aNewCodeOwner isNil not and: [
			(aNewCodeOwner == aSubject) not ])  ifTrue: [
			anEditor isNil ifFalse: [
				(Delay forMilliseconds: 500) wait.
				anEditor tryToSetSelectionOn: aNewCodeOwner ]]] fork!

setValueWithController: theValue


	subject isNil ifTrue: [^nil].

	^Object messageNotUnderstoodSignal
		handle: [:anException | anException returnWith: (super setValue: theValue)]
		do: [ 	| aMetaInfo |
			editor codeEditorController view displayContents text: theValue.

			aMetaInfo := self metaInfo.
			aMetaInfo isNil ifTrue: [ ^nil].
			aMetaInfo  object: subject setTC: theValue "withController: 
				(editor isNil ifTrue: [nil] ifFalse: [editor codeEditorController])"
		]! !

!CMEnumAspectAdaptorWithCheck publicMethodsFor: 'accessing'!

setValueUsingTarget: theObject to: theValue
	| aValue aFeatureMetaInfo aReferencedTypeMetaInfo someEnumAttributes aNewObject |

	theValue isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aReferencedTypeMetaInfo := aFeatureMetaInfo referencedType.
	aReferencedTypeMetaInfo isEnumeration ifFalse: [ ^nil].
 
	someEnumAttributes := aReferencedTypeMetaInfo allEnumerationAttributes.
	(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].

	(someEnumAttributes detect: [:anAttribute | anAttribute name = theValue] ifNone: [ nil]) isNil ifTrue: [ ^nil].
	
	aNewObject := aReferencedTypeMetaInfo createEnumObject: theValue.
	aNewObject isNil ifTrue: [  ^nil].

	aValue := aFeatureMetaInfo object: theObject setTC: aNewObject.
	^aValue!

valueUsingTarget: theObject
	"Answer the value returned by sending the receiver's retrieval (get)  selector to anObject"

	| aMetaInfo aEnumValue aEnumAttribute |
	theObject == nil ifTrue: [^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aEnumValue := aMetaInfo getObjectFeatureValueTC: theObject.
	aEnumValue isNil ifTrue: [ ^nil].

	aEnumAttribute := aEnumValue enumValue.
	aEnumAttribute isNil ifTrue: [ ^nil].

	^aEnumAttribute name! !

!CMOperationAdaptor publicMethodsFor: 'accessing'!

setValue: theValue 

	| aValue |
self halt.
	self isReadOnly ifTrue: [ 
		Dialog warn: 'This Operation is not available'.
		^nil].

	aValue := self setValueUsingTarget: self target to: theValue.
	^aValue!

setValueUsingTarget: theObject to: theValue
	"Set the value of anObject by sending the receiver's 
	store (put) selector to the anObject with argument newValue."

	| aMetaInfo aValue |
	theObject == nil ifTrue: [^nil].
self halt.
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo performObjectOperationTC: theObject.
	^aValue! !

!CMTerminalAspectAdaptorWithCheck class publicMethodsFor: 'instance creation'!

newWithMetaInfo: theMetaInfo
	| anAdaptor |
	anAdaptor := self new.
	anAdaptor metaInfo: theMetaInfo.
	^anAdaptor! !

!CMTerminalAspectAdaptorWithCheck publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo!

setValue: theValue 
	

	[
		| aValue aNewValue anEditor aMetaInfo | 
		aValue := self value.
		anEditor  := editor editorToTryToSetSelection.

		aMetaInfo := self metaInfo.
		aMetaInfo isNil ifTrue: [ ^nil].
		aNewValue := aMetaInfo  object: subjectChannel parent parent value setTC: theValue. 
		(aNewValue isNil not and: [
			(aNewValue == aValue) not ])  ifTrue: [
			anEditor isNil ifFalse: [
				(Delay forMilliseconds: 500) wait.
				anEditor tryToSetSelectionOn: aNewValue ]]] fork! !

CMAspectAdaptorWithCheck initializeAfterLoad!
CMEnumAspectAdaptorWithCheck initializeAfterLoad!
CMOperationAdaptor initializeAfterLoad!
CMClassAspectAdaptorWithCheck initializeAfterLoad!
CMCodeAspectAdaptorWithCheck initializeAfterLoad!
CMTerminalAspectAdaptorWithCheck initializeAfterLoad!
CODE_META_Adaptors initializeAfterLoad!

CODE_META_Adaptors loaded!
