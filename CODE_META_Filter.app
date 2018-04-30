'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Filter in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Filter becomeDefault!

METASimpleFilter subclass: #CMSimpleFilter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Filter becomeDefault!

METASimpleFilterSpec subclass: #CMSimpleFilterSpec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Filter becomeDefault!

METASimpleTest subclass: #CMSimpleTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Filter becomeDefault!

METASimpleTestSpec subclass: #CMSimpleTestSpec
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Filter becomeDefault!

SubApplication subclass: #CODE_META_Filter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Filter becomeDefault!

!CMSimpleFilter class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMSimpleFilterSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMSimpleTest class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMSimpleTest publicMethodsFor: 'filtering'!

testValue: theValue inNode: theNode

	| aChildSpec aCurrentValue aMatch |

	theValue isNil ifTrue: [^false].

	self enabled ifFalse: [^true].
	spec isNil ifTrue: [^true].

	aChildSpec := spec childSpec.
	aChildSpec isNil ifTrue: [^true].

	aCurrentValue := aChildSpec getValueFor: theValue.
	aMatch := self matchValue: aCurrentValue  inNode: theNode.

	self includeMode = spec class includeModeExclude ifTrue: [
		aMatch := aMatch not].

	^aMatch! !

!CMSimpleTestSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

CMSimpleFilter initializeAfterLoad!
CMSimpleFilterSpec initializeAfterLoad!
CMSimpleTest initializeAfterLoad!
CMSimpleTestSpec initializeAfterLoad!
CODE_META_Filter initializeAfterLoad!

CODE_META_Filter loaded!
