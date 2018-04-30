'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META_DefinedUI createSubApplication: #CODE_META_DefinedUI_Holder in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_DefinedUI_Holder becomeDefault!

Object subclass: #CMDefinedUIPersistencyHolder
	classInstanceVariableNames: 'currentDefinedUIs '
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Holder becomeDefault!

SubApplication subclass: #CODE_META_DefinedUI_Holder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Holder becomeDefault!

!CMDefinedUIPersistencyHolder class publicMethodsFor: 'all'!

resetAllCurrentDefinedUIs
	"CMDefinedUIPersistencyHolder resetAllCurrentDefinedUIs"
	(Dialog confirm: 'Do you really want to reset Current DefinedUIs in all SubClasses ?' initialAnswer: false) ifFalse: [ ^self].

	self resetAllCurrentDefinedUIsNoDialog!

resetAllCurrentDefinedUIsNoDialog
	"CMDefinedUIPersistencyHolder resetAllCurrentDefinedUIs"

	self withAllSubclassesDo: [:aTPC | aTPC  resetCurrentDefinedUIs].! !

!CMDefinedUIPersistencyHolder class publicMethodsFor: 'current'!

currentDefinedUI
	"self  currentDefinedUI browse"

	| aDefaultCurrentDefinedUISelector |

	aDefaultCurrentDefinedUISelector := self defaultCurrentDefinedUISelector.
	aDefaultCurrentDefinedUISelector isNil ifTrue: [ ^nil].

	^self currentDefinedUIStoreMethodSelector: aDefaultCurrentDefinedUISelector!

currentDefinedUIBoundToModel
	"self  currentDefinedUIBoundToModel browse"

	| aDefaultCurrentDefinedUISelector |

	aDefaultCurrentDefinedUISelector := self defaultCurrentDefinedUISelector.
	aDefaultCurrentDefinedUISelector isNil ifTrue: [ ^nil].

	^self currentDefinedUIStoreMethodSelectorBoundToModel: aDefaultCurrentDefinedUISelector!

currentDefinedUIs
	"DefinedUIsPersistencyHolder  currentDefinedUIs"

	currentDefinedUIs isNil  ifTrue: [ self resetCurrentDefinedUIs].
	^currentDefinedUIs!

currentDefinedUIStoreMethodSelector: theStoreMethodSelector
	"self  currentDefinedUIStoreMethodSelector: self defaultCurrentDefinedUISelector"
	
	| aDefinedUI someCurrentDefinedUIs |

	aDefinedUI := nil.

	someCurrentDefinedUIs := self currentDefinedUIs.

	aDefinedUI :=  someCurrentDefinedUIs at:  theStoreMethodSelector ifAbsent: [ nil].

	aDefinedUI isNil ifFalse: [ ^aDefinedUI].

	aDefinedUI := self retrieveCurrentDefinedUIStoreMethodSelector: theStoreMethodSelector.
	aDefinedUI isNil ifTrue: [ ^nil].

	someCurrentDefinedUIs at:  theStoreMethodSelector  put: aDefinedUI.

	^aDefinedUI!

currentDefinedUIStoreMethodSelectorBoundToModel: theStoreMethodSelector
	"self  currentDefinedUIStoreMethodSelector: self defaultCurrentDefinedUISelector"
	
	| aDefinedUI someCurrentDefinedUIs |

	aDefinedUI := nil.

	someCurrentDefinedUIs := self currentDefinedUIs.

	aDefinedUI :=  someCurrentDefinedUIs at:  theStoreMethodSelector ifAbsent: [ nil].

	aDefinedUI isNil ifFalse: [ 
		aDefinedUI model isNil ifTrue: [ aDefinedUI definedWindow rebindToModel].
		^aDefinedUI
	].

	aDefinedUI := self retrieveCurrentDefinedUIStoreMethodSelector: theStoreMethodSelector.
	aDefinedUI isNil ifTrue: [ ^nil].

	aDefinedUI definedWindow rebindToModel.

	someCurrentDefinedUIs at:  theStoreMethodSelector  put: aDefinedUI.

	^aDefinedUI!

resetCurrentDefinedUI
	"self  resetCurrentDefinedUI"

	^self resetCurrentDefinedUIsStoreMethodSelector: self defaultCurrentDefinedUISelector!

resetCurrentDefinedUIs
	"self  resetCurrentDefinedUIs"

	currentDefinedUIs := IdentityDictionary new.!

resetCurrentDefinedUIsStoreMethodSelector: theStoreMethodSelector
	"self resetCurrentDefinedUIsStoreMethodSelector: self defaultCurrentDefinedUISelector "
	

	currentDefinedUIs isNil  ifTrue: [ ^self].

	currentDefinedUIs removeKey:  theStoreMethodSelector ifAbsent: [ nil]!

retrieveCurrentDefinedUIStoreMethodSelector: theStoreMethodSelector
	"self  currentDefinedUIStoreMethodSelector: self defaultCurrentDefinedUISelector"
	
	| somePersistenceValues aDefinedUI |

	somePersistenceValues := nil.
	Object messageNotUnderstoodSignal 
		handle: [:anEx | ] 
		do: [ somePersistenceValues := self perform:  theStoreMethodSelector].
	(somePersistenceValues isNil or: [ somePersistenceValues isEmpty]) ifTrue: [ ^nil].

 	aDefinedUI := CMDefinedPart newFromPersistenceAsCode: somePersistenceValues.
	^aDefinedUI! !

!CMDefinedUIPersistencyHolder class publicMethodsFor: 'default'!

defaultCurrentDefinedUISelector
	"self  defaultCurrentDefinedUISelector "

	^nil! !

!CMDefinedUIPersistencyHolder class publicMethodsFor: 'ojo'!

ojoCMDefinedParts!

ojoDefinedUI! !

CMDefinedUIPersistencyHolder initializeAfterLoad!
CODE_META_DefinedUI_Holder initializeAfterLoad!

CODE_META_DefinedUI_Holder loaded!
