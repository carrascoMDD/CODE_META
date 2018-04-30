'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CM_Info_Holder in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CM_Info_Holder becomeDefault!

Object subclass: #CMInfoPersistencyHolder
	classInstanceVariableNames: 'currentInfos '
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CM_Info_Holder becomeDefault!

SubApplication subclass: #CM_Info_Holder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CM_Info_Holder becomeDefault!

!CMInfoPersistencyHolder class publicMethodsFor: 'all'!

resetAllCurrentInfos
	"CMInfoPersistencyHolder resetAllCurrentInfos"
	(Dialog confirm: 'Do you really want to reset Current Infos in all SubClasses ?' initialAnswer: false) ifFalse: [ ^self].

	self resetAllCurrentInfosNoDialog!

resetAllCurrentInfosNoDialog
	"CMInfoPersistencyHolder resetAllCurrentInfos"

	self withAllSubclassesDo: [:aTPC | aTPC  resetCurrentInfos].! !

!CMInfoPersistencyHolder class publicMethodsFor: 'current'!

currentInfo
	"self  currentInfo browsePath"

	| aDefaultCurrentInfoSelector |

	aDefaultCurrentInfoSelector := self defaultCurrentInfoSelector.
	aDefaultCurrentInfoSelector isNil ifTrue: [ ^nil].

	^self currentInfoStoreMethodSelector: aDefaultCurrentInfoSelector!

currentInfos
	"InfosPersistencyHolder  currentInfos"

	currentInfos isNil  ifTrue: [ self resetCurrentInfos].
	^currentInfos!

currentInfoStoreMethodSelector: theStoreMethodSelector
	"self  currentInfoStoreMethodSelector: self defaultCurrentInfoSelector"
	
	| someCurrentInfos anInfo |

	theStoreMethodSelector isNil ifTrue: [ ^nil].
 
	anInfo := nil.

	someCurrentInfos := self currentInfos.

	anInfo :=  someCurrentInfos at:  theStoreMethodSelector ifAbsent: [ nil].

	anInfo isNil ifFalse: [ ^anInfo].

	anInfo :=  Object messageNotUnderstoodSignal 
		handle: [:anEx | 
			anEx parameter selector = theStoreMethodSelector 
				ifTrue: [ anEx returnWith: nil] 
				ifFalse: [ anEx reject]
		] 
		do: [ self perform:  theStoreMethodSelector].
	anInfo isNil ifTrue: [ ^nil].

	someCurrentInfos at:  theStoreMethodSelector  put: anInfo.

	^anInfo!

hasCurrentInfo

	"self  hasCurrentInfo: self currentInfo"

	| aDefaultCurrentInfoSelector |

	aDefaultCurrentInfoSelector := self defaultCurrentInfoSelector.
	aDefaultCurrentInfoSelector isNil ifTrue: [ ^false].

	^self hasCurrentInfoStoreMethodSelector: aDefaultCurrentInfoSelector!

hasCurrentInfo: theObject

	"self  hasCurrentInfo: self currentInfo"

	| aDefaultCurrentInfoSelector |

	aDefaultCurrentInfoSelector := self defaultCurrentInfoSelector.
	aDefaultCurrentInfoSelector isNil ifTrue: [ ^false].

	^self hasCurrentInfo: theObject storeMethodSelector: aDefaultCurrentInfoSelector!

hasCurrentInfo: theObject storeMethodSelector: theStoreMethodSelector
	"self  hasCurrentInfoStoreMethodSelector: self defaultCurrentInfoSelector"
	
	| someCurrentInfos anInfo |
	
	theObject isNil ifTrue: [ ^false].
	anInfo := nil.

	someCurrentInfos := self currentInfos.

	anInfo :=  someCurrentInfos at:  theStoreMethodSelector ifAbsent: [ nil].
	^anInfo == theObject!

hasCurrentInfoStoreMethodSelector: theStoreMethodSelector
	"self  hasCurrentInfoStoreMethodSelector: self defaultCurrentInfoSelector"
	
	| someCurrentInfos anInfo |

	anInfo := nil.

	someCurrentInfos := self currentInfos.

	anInfo :=  someCurrentInfos at:  theStoreMethodSelector ifAbsent: [ nil].
	^anInfo isNil not!

resetCurrentInfo
	"self  resetCurrentInfo"

	^self resetCurrentInfosStoreMethodSelector: self defaultCurrentInfoSelector!

resetCurrentInfos
	"self  resetCurrentInfos"

	currentInfos := IdentityDictionary new.!

resetCurrentInfosStoreMethodSelector: theStoreMethodSelector
	"self resetCurrentInfosStoreMethodSelector: self defaultCurrentInfoSelector "
	
	currentInfos isNil  ifTrue: [ ^self].

	currentInfos removeKey:  theStoreMethodSelector ifAbsent: [ nil]! !

!CMInfoPersistencyHolder class publicMethodsFor: 'default'!

defaultCurrentInfoSelector
	"self  defaultCurrentInfoSelector "

	^nil! !

!CMInfoPersistencyHolder class publicMethodsFor: 'ojo'!

ojoInfo! !

CMInfoPersistencyHolder initializeAfterLoad!
CM_Info_Holder initializeAfterLoad!

CM_Info_Holder loaded!
