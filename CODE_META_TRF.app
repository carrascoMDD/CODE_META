'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_TRF in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_TRF becomeDefault!

Object subclass: #CMSystemPrimitiveBroker
	classInstanceVariableNames: 'brokers '
	instanceVariableNames: 'name primitiveClass isTypeOfBlock isNonEmptyBlock createBlock createFromStringBlock createFromObjectBlock cloneBlock specTypeBlock nameStringFromObjectBlock formatString widgetType '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

Object subclass: #CMTypeDependency
	instanceVariableNames: 'type feature firstObservers path '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

Object subclass: #CMTypeObserver
	instanceVariableNames: 'observedType observedFeature dependency previousObserver nextObservers index path '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

Transaction subclass: #CMTransaction
	instanceVariableNames: 'mustPropagateChangeEventsOnCommit '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

PropertyListDictionary variableSubclass: #CMGenericObject
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

BiLink subclass: #CMGenericLinkMaker
	instanceVariableNames: 'transaction linkState result undoResult oneObject otherObject oneBefore otherBefore doOrdered doUnlink metaInfo oneSaved otherSaved movement '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

CMGenericLinkMaker subclass: #CMGenericChangeEventsMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

CMGenericChangeEventsMaker subclass: #CMGenericDeletionChangeEventsMaker
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

TransactionNester subclass: #CMTransactionNester
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

SubApplication subclass: #CODE_META_TRF
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_TRF becomeDefault!

!CMGenericChangeEventsMaker class publicMethodsFor: 'instance creation'!

from: elOne before: elOneBefore to: elOther before: elOtherBefore metaInfo: theMetaInfo
	self shouldNotImplement!

from: elOne before: elBefore to: elOther metaInfo: theMetaInfo
	self shouldNotImplement!

from: elOne to: elOther metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self insideFrom: elOne to: elOther metaInfo: theMetaInfo
	]!

from: elOne to: elOther move: theMovement before: theBefore metaInfo: theMetaInfo
	self shouldNotImplement!

from: elOne to: elOther move: theMovement metaInfo: theMetaInfo
	self shouldNotImplement!

insideFrom:  elOne before: elOneBefore to: elOther before: elOtherBefore metaInfo: theMetaInfo
		
	self shouldNotImplement!

insideFrom:  elOne before:	elBefore to: elOther metaInfo: theMetaInfo
		
	self shouldNotImplement!

insideFrom: elOne to: elOther metaInfo: theMetaInfo
	
	| unCMGenericChangeEventsMaker |

	unCMGenericChangeEventsMaker := super new initialize one: elOne to: elOther metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMGenericChangeEventsMaker doAction == unCMGenericChangeEventsMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true!

insideFrom: elOne to: elOther move: theMovement before: theBefore metaInfo: theMetaInfo
	
	self shouldNotImplement!

insideFrom: elOne to: elOther move: theMovement metaInfo: theMetaInfo
	
	self shouldNotImplement!

unlinkerFrom: elOne to: elOther metaInfo: theMetaInfo
	self shouldNotImplement!

unlinkerInsideFrom: elOne to: elOther metaInfo: theMetaInfo
	
	self shouldNotImplement! !

!CMGenericChangeEventsMaker class publicMethodsFor: 'linkable classes'!

topAbstractElementClasses
	^Array with: PROEntity! !

!CMGenericChangeEventsMaker class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences!

preferredTransactionClass
	^self preferredPreferencesClass preferredTransactionClass! !

!CMGenericChangeEventsMaker publicMethodsFor: 'change events'!

propagateChangeEventsChangeEntitiesInto: theChangedEntities

	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aMetaInfo propagateChangeEventsFromLinker: self! !

!CMGenericChangeEventsMaker publicMethodsFor: 'linking'!

doAction

	self  state: #previousState.
	self  state: #testingState.
	self  state: #uniqTestingState.
	self  state: #savingState.

	self  state: #registeringState.
	self preferredTransactionClass register: self.

	self  state: # doingState.
	self  state: #doneState.!

save: theValueToSave side: theSide!

undoAction

	self  state: #undoingState.
	self state: #undonedState.

	self releaseAndChain! !

!CMGenericChangeEventsMaker publicMethodsFor: 'multistep linking'!

subActionPreviousToDoLink
	
	^true!

subActionSaveToDoLink
	
	^true!

subActionTestToDoLink
	
	^true!

subActionToDoLink
	
	^true!

subActionToUndoLink

	^true!

subActionUniqTestToDoLink
	
	^true! !

!CMGenericChangeEventsMaker publicMethodsFor: 'write set'!

collectChangedEntitiesInto: theChangedEntities

	| someEntityClasses |
	someEntityClasses := self class topAbstractElementClasses.
	(someEntityClasses isNil or: [ someEntityClasses isEmpty]) ifTrue: [ ^self].
	
	(Array with: oneObject with: otherObject) do: [:anObj |	
		(someEntityClasses detect: [:aClass | anObj isKindOf: aClass] ifNone: [ nil]) isNil  ifFalse: [ 
			theChangedEntities add: anObj
		]
	]! !

!CMGenericDeletionChangeEventsMaker class publicMethodsFor: 'instance creation'!

delete: elOne metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self insideDelete: elOne metaInfo: theMetaInfo
	]!

from: elOne to: elOther metaInfo: theMetaInfo
	self shouldNotImplement!

insideDelete: elOne metaInfo: theMetaInfo
	| unCMGenericDeletionChangeEventsMaker |

	unCMGenericDeletionChangeEventsMaker := super new initialize one: elOne to: nil metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMGenericDeletionChangeEventsMaker doAction == unCMGenericDeletionChangeEventsMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true!

insideFrom: elOne to: elOther metaInfo: theMetaInfo

	self shouldNotImplement! !

!CMGenericDeletionChangeEventsMaker publicMethodsFor: 'change events'!

propagateChangeEventsChangeEntitiesInto: theChangedEntities

	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aMetaInfo propagateDeletionChangeEventsFromLinker: self! !

!CMGenericLinkMaker class publicMethodsFor: 'instance creation'!

from: elOne before: elOneBefore to: elOther before: elOtherBefore metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self insideFrom: elOne before: elOneBefore to: elOther before: elOtherBefore metaInfo: theMetaInfo
	]!

from: elOne before: elBefore to: elOther metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self insideFrom: elOne before: elBefore to: elOther metaInfo: theMetaInfo
	]!

from: elOne to: elOther metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self insideFrom: elOne to: elOther metaInfo: theMetaInfo
	]!

from: elOne to: elOther move: theMovement before: theBefore metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self insideFrom: elOne to: elOther move: theMovement before: theBefore metaInfo: theMetaInfo
	]!

from: elOne to: elOther move: theMovement metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self insideFrom: elOne to: elOther move: theMovement metaInfo: theMetaInfo
	]!

insideFrom:  elOne before: elOneBefore to: elOther before: elOtherBefore metaInfo: theMetaInfo
		
	| unCMLinkMaker |
	unCMLinkMaker := super new initialize one: elOne before: elOneBefore to: elOther before: elOtherBefore metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMLinkMaker doAction == unCMLinkMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true!

insideFrom:  elOne before:	elBefore to: elOther metaInfo: theMetaInfo
		
	| unCMLinkMaker |
	unCMLinkMaker := super new initialize one: elOne before: elBefore to: elOther metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMLinkMaker doAction == unCMLinkMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true!

insideFrom: elOne to: elOther metaInfo: theMetaInfo
	
	| unCMGenericLinkMaker |

	unCMGenericLinkMaker := super new initialize one: elOne to: elOther metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMGenericLinkMaker doAction == unCMGenericLinkMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true!

insideFrom: elOne to: elOther move: theMovement before: theBefore metaInfo: theMetaInfo
	
	| unCMGenericLinkMaker |

	unCMGenericLinkMaker := super new initialize one: elOne to: elOther move: theMovement before: theBefore metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMGenericLinkMaker doAction == unCMGenericLinkMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true!

insideFrom: elOne to: elOther move: theMovement metaInfo: theMetaInfo
	
	| unCMGenericLinkMaker |

	unCMGenericLinkMaker := super new initialize one: elOne to: elOther move: theMovement metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMGenericLinkMaker doAction == unCMGenericLinkMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true!

unlinkerFrom: elOne to: elOther metaInfo: theMetaInfo
	^self preferredTransactionClass newTransactionDo: [
		self unlinkerInsideFrom: elOne to: elOther metaInfo: theMetaInfo
	]!

unlinkerInsideFrom: elOne to: elOther metaInfo: theMetaInfo
	
	| unCMGenericLinkMaker |

	unCMGenericLinkMaker := super new initialize unlinkOne: elOne to: elOther metaInfo: theMetaInfo.

	(elOne isNil not and: [ unCMGenericLinkMaker doAction == unCMGenericLinkMaker ]) ifFalse: [ 
		self preferredTransactionClass undoLastTransaction. 
		^false
	].
	^true! !

!CMGenericLinkMaker class publicMethodsFor: 'linkable classes'!

topAbstractElementClasses
	^Array with: PROEntity with: CMGenericObject! !

!CMGenericLinkMaker class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences!

preferredTransactionClass
	^self preferredPreferencesClass preferredTransactionClass! !

!CMGenericLinkMaker publicMethodsFor: 'accessing'!

doMove
	^movement notNil!

doOrdered
	^doOrdered == true!

doUnlink
	^doUnlink == true!

metaInfo
	^metaInfo!

movement
	^movement!

oneBefore
	^oneBefore!

oneObject
	^oneObject!

oneSaved
	^oneSaved!

otherBefore
	^otherBefore!

otherObject
	^otherObject!

otherSaved
	^otherSaved! !

!CMGenericLinkMaker publicMethodsFor: 'change events'!

propagateChangeEventsChangeEntitiesInto: theChangedEntities

	| aMetaInfo |
	self collectChangedEntitiesInto: theChangedEntities.

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aMetaInfo propagateChangeEventsFromLinker: self! !

!CMGenericLinkMaker publicMethodsFor: 'initialization'!

initialize
	transaction			:= nil.
	linkState				:= OrderedCollection new.
	result					:= nil.
	undoResult			:= nil.
	oneObject 			:= nil.
	oneBefore			:= nil.
	otherObject 			:= nil.
	otherBefore			:= nil.
	doOrdered 			:= false.
	doUnlink 				:= false.
	metaInfo 				:= nil.
	oneSaved 			:= nil.
	otherSaved 			:= nil.
	movement			:= nil.!

one: elOne before: elOneBefore to: elOther before: elOtherBefore metaInfo: theMetaInfo

	oneObject 		:= elOne.
	oneBefore		:= elOneBefore.
	otherObject 		:= elOther.
	otherBefore		:= elOtherBefore.
	metaInfo 			:= theMetaInfo.
	doOrdered 		:= true.
	doUnlink 			:= false.
	movement		:= nil.

	self state: #paramsReceivedState.!

one: elOne before: elBefore to: elOther metaInfo: theMetaInfo

	oneObject 		:= elOne.
	oneBefore		:= elBefore.
	otherObject 		:= elOther.
	otherBefore		:= nil.
	metaInfo 			:= theMetaInfo.
	doOrdered 		:= true.
	doUnlink 			:= false.
	movement		:= nil.

	self state: #paramsReceivedState.!

one: elOne to: elOther metaInfo: theMetaInfo

	oneObject 		:= elOne.
	oneBefore		:= nil.
	otherObject 		:= elOther.
	otherBefore		:= nil.
	metaInfo 			:= theMetaInfo.
	doOrdered 		:= false.
	doUnlink 			:= false.
	movement		:= nil.

	self state: #paramsReceivedState.!

one: elOne to: elOther move: theMovement before: theBefore metaInfo: theMetaInfo

	oneObject 		:= elOne.
	oneBefore		:= theBefore.
	otherObject 		:= elOther.
	otherBefore		:= nil.
	metaInfo 			:= theMetaInfo.
	doOrdered 		:= false.
	doUnlink 			:= false.
	movement		:= theMovement.

	self state: #paramsReceivedState.!

one: elOne to: elOther move: theMovement metaInfo: theMetaInfo

	oneObject 		:= elOne.
	oneBefore		:= nil.
	otherObject 		:= elOther.
	otherBefore		:= nil.
	metaInfo 			:= theMetaInfo.
	doOrdered 		:= false.
	doUnlink 			:= false.
	movement		:= theMovement.

	self state: #paramsReceivedState.!

release
	transaction			:= nil.
	linkState				:= nil.
	linkState				:= nil.
	result					:= nil.
	undoResult			:= nil.
	oneObject 			:= nil.
	oneBefore			:= nil.
	otherObject 			:= nil.
	otherBefore			:= nil.
	doOrdered 			:= nil.
	doUnlink 				:= nil.
	metaInfo 				:= nil.
	oneSaved 			:= nil.
	otherSaved 			:= nil.
	movement			:= nil.

	super release!

releaseAndChain
	self release!

unlinkOne: elOne to: elOther metaInfo: theMetaInfo

	oneObject 		:= elOne.
	oneBefore		:= nil.
	otherObject 		:= elOther.
	otherBefore		:= nil.
	metaInfo 			:= theMetaInfo.
	doOrdered 		:= false.
	doUnlink 			:= true.

	self state: #paramsReceivedState.! !

!CMGenericLinkMaker publicMethodsFor: 'linking'!

doAction

	self  state: #previousState.
	self subActionPreviousToDoLink  ifFalse: [
		self  state: #previousFailedState.
		^nil
	].

	self  state: #testingState.
	self subActionTestToDoLink ifFalse: [
		self  state: #testFailedState.
		^nil
	].

	self  state: #uniqTestingState.
	self subActionUniqTestToDoLink ifFalse: [
		self  state: #uniqTestFailedState.
		^nil
	].

	self  state: #savingState.
	self subActionSaveToDoLink ifFalse: [
		 self saveFailedState.
		^nil
	].

	self  state: #registeringState.
	self preferredTransactionClass register: self.

	self  state: # doingState.
	(result := self subActionToDoLink) ifFalse: [
		self  state: # badResultState.
		^nil 
	].
	self  state: #doneState.!

save: theValueToSave side: theSide

	theSide = #one 
		ifTrue: [ oneSaved := theValueToSave] 
		ifFalse: [ otherSaved := theValueToSave].!

undoAction

	self  state: #undoingState.
	undoResult := self subActionToUndoLink.
	self state: #undonedState.

	self releaseAndChain! !

!CMGenericLinkMaker publicMethodsFor: 'multistep linking'!

subActionPreviousToDoLink
	
	^self metaInfo subActionPreviousToDoLink: self!

subActionSaveToDoLink
	
	^self metaInfo subActionSaveToDoLink: self!

subActionTestToDoLink
	
	^self metaInfo subActionTestToDoLink: self!

subActionToDoLink
	
	^self metaInfo subActionToDoLink: self!

subActionToUndoLink

	^self metaInfo subActionToUndoLink: self!

subActionUniqTestToDoLink
	
	^self metaInfo subActionUniqTestToDoLink: self! !

!CMGenericLinkMaker publicMethodsFor: 'preferences'!

preferredTransactionClass
	^self class preferredTransactionClass! !

!CMGenericLinkMaker publicMethodsFor: 'printing'!

printOn: elStream
	elStream nextPutAll: (self class printString , ' ', self stateString)! !

!CMGenericLinkMaker publicMethodsFor: 'states'!

state
	^linkState isEmpty
		ifTrue: [#unusedState]
		ifFalse: [ linkState last]!

state: elStateSymbol
	^linkState  addLast: elStateSymbol!

stateString
	^self state asString! !

!CMGenericLinkMaker publicMethodsFor: 'transactions'!

transaction
	^transaction!

transaction: laTransaction	
	^transaction := laTransaction! !

!CMGenericLinkMaker publicMethodsFor: 'write set'!

collectChangedEntitiesInto: theChangedEntities

	| someEntityClasses |
	someEntityClasses := self class topAbstractElementClasses.
	(someEntityClasses isNil or: [ someEntityClasses isEmpty]) ifTrue: [ ^self].
	
	(Array with: oneObject with: otherObject) do: [:anObj |	
		(someEntityClasses detect: [:aClass | anObj isKindOf: aClass] ifNone: [ nil]) isNil  ifFalse: [ 
			theChangedEntities add: anObj
		]
	]! !

!CMGenericObject class publicMethodsFor: 'boss'!

bossInFromFileNamed: theFileName
	"CMGenericObject  bossInFromFileNamed: 'Diseases.boss'"
"self  recursiveDetachFromMetaInfoIn: (IdentityDictionary new: 553)

self bossOutToFileNamed: 'Diseases.boss'

self recursiveAttachToMetaInfoIn: (IdentityDictionary new: 553) 
model: ( KRSimpleMetaInfoHolder   currentModelStoreMethodSelector: #kronoSimpleStore)

CMGenericObject  bossInFromFileNamed: 'Diseases.boss'"
	| unFilename aBoss anObj |

	theFileName isNil ifTrue: [ ^nil].

	unFilename := theFileName asFilename.

	[
		aBoss := BinaryObjectStorage onOld: (unFilename withEncoding: #binary) readStream.
		anObj := aBoss next.
	]
		valueNowOrOnUnwindDo: 
	[
		aBoss isNil ifFalse: [ aBoss close]
	].

	(anObj isKindOf: self) ifFalse: [ ^nil].
	^anObj! !

!CMGenericObject class publicMethodsFor: 'constants'!

detachedEnumValuePropertyName
	^#detachedEnumValueCMGO!

detachedMetaInfoPropertyName
	^#detachedMetaInfoCMGO!

domainModelPropertyName
	^#domainModelCMGO!

enumValuePropertyName
	^#enumValueCMGO!

homeIDCounterPropertyName
	^#homeIDCounterCMGO!

homeRootsPropertyName
	^#homeRootsCMGO!

metaInfoPropertyName
	^#metaInfoCMGO!

synthesisCachePropertyName
	^#synthesisCacheCMGO! !

!CMGenericObject class publicMethodsFor: 'example'!

example01
	"CMGenericObject example01"

	| aModel aTypeSpecification aRelationshipSubPackages aTypePackage aSpecificationMusicEquipment aTypeObject aTypeElement anAttributeIdentifier aPackageStereo aRelationshipObjects anObjectAmplifier |

	aModel := CODEElement newFromPersistenceAsCode: CMViewModel exampleCODEViewModelForCMViews.

	aTypeElement := aModel resolveOrNewReferencedTypeName: 'Element' moduleNames: #('Core').
	anAttributeIdentifier := aTypeElement attributeNamed: 'identifier'.
	aTypeSpecification := aModel resolveOrNewReferencedTypeName: 'Specification' moduleNames: #('Core').
	aTypePackage := aModel resolveOrNewReferencedTypeName: 'Package' moduleNames: #('Core').
	aRelationshipSubPackages := aTypePackage relationshipNamed: 'subPackages'.
	aTypeObject := aModel resolveOrNewReferencedTypeName: 'Object' moduleNames: #('Core').
	aRelationshipObjects := aTypePackage relationshipNamed: 'objects'.
	
	aSpecificationMusicEquipment := CMGenericObject newWithMetaInfo: aTypeSpecification.
	anAttributeIdentifier object: aSpecificationMusicEquipment setTC: 'MusicEquipment'.

	aPackageStereo := CMGenericObject newWithMetaInfo: aTypePackage.
	anAttributeIdentifier object: aPackageStereo setTC: 'Stereo'.

	aRelationshipSubPackages object: aSpecificationMusicEquipment addTC: aPackageStereo.

	anObjectAmplifier := CMGenericObject newWithMetaInfo: aTypeObject.
	anAttributeIdentifier object: anObjectAmplifier setTC: 'Amplifier'.

	aRelationshipObjects object: aPackageStereo addTC: anObjectAmplifier.


	^aSpecificationMusicEquipment!

example02
	"CMGenericObject example02"

	| aModel  aTypeEHCRRoot aEHCRRoot |

	aModel := PROTOprENV13606MetaInfoHolder  currentModel. "browsePath."
	"aModel := CODEElement newFromPersistenceAsCode: PROTOprENV13606MetaInfoHolder prENV13606Store."

	aTypeEHCRRoot := aModel resolveOrNewReferencedTypeName: 'EHCRRootArchitecturalComponent' moduleNames: #('Core').
	
	aEHCRRoot := CMGenericObject newWithMetaInfo: aTypeEHCRRoot.

	^aEHCRRoot! !

!CMGenericObject class publicMethodsFor: 'instance creation'!

newWithMetaInfo: theMetaInfo

	| anObject |
	anObject := self new initWithMetaInfo: theMetaInfo.
	^anObject!

newWithMetaInfo: theMetaInfo enumValue: theEnumValue

	| anObject |
	anObject := self new initWithMetaInfo: theMetaInfo enumValue: theEnumValue.
	^anObject! !

!CMGenericObject class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMGenericObject publicMethodsFor: 'accessing'!

enumValue
	^self propertyAt: self class enumValuePropertyName!

initSynthesisCache
	| aSynthesisCache  |
	aSynthesisCache := IdentityDictionary new: 7.
	self propertyAt: self class synthesisCachePropertyName put: aSynthesisCache.
	^aSynthesisCache!

metaInfo
	| aMetaInfoOrReference aDomain |
	aMetaInfoOrReference := self propertyAt: self class metaInfoPropertyName.
	aMetaInfoOrReference  isNil ifTrue: [ ^nil].
	(aMetaInfoOrReference isKindOf: CODEElement) ifTrue: [ ^aMetaInfoOrReference].

	(aMetaInfoOrReference isKindOf: Array) ifFalse: [ ^nil].

	aDomain := self getDomain.
	aDomain isNil ifTrue: [ ^nil].

	^aDomain resolveMetaInfoReference: aMetaInfoOrReference!

synthesisCache
	| aSynthesisCache  |
	aSynthesisCache := self propertyAt: self class synthesisCachePropertyName.
	^aSynthesisCache! !

!CMGenericObject publicMethodsFor: 'bindings'!

bindMetaInfo
	| aMetaInfoOrReference aDomain aMetaInfo |
	aMetaInfoOrReference := self propertyAt: self class metaInfoPropertyName.
	aMetaInfoOrReference  isNil ifTrue: [ ^self].

	(aMetaInfoOrReference isKindOf: CODEElement) ifTrue: [ ^self].
	(aMetaInfoOrReference isKindOf: Array) ifFalse: [ ^self].

	aDomain := self getDomain.
	aDomain isNil ifTrue: [ ^self].

	aMetaInfo := aDomain resolveMetaInfoReference: aMetaInfoOrReference.
	aMetaInfo  isNil ifTrue: [ ^self].
	(aMetaInfo isKindOf: CODEElement) ifTrue: [ ^self].

	self propertyAt: self class metaInfoPropertyName  put: aMetaInfo!

unBindMetaInfo
	| aMetaInfoOrReference aDomain aMetaInfoReference |
	aMetaInfoOrReference := self propertyAt: self class metaInfoPropertyName.
	aMetaInfoOrReference  isNil ifTrue: [ ^self].
	(aMetaInfoOrReference isKindOf: CODEElement) ifFalse: [ ^self].

	aDomain := self getDomain.
	aDomain isNil ifTrue: [ ^self].

	aMetaInfoReference := aDomain referenceForMetaInfo: aMetaInfoOrReference.
	aMetaInfoReference isNil ifTrue: [ ^self].

	self propertyAt: self class metaInfoPropertyName put: aMetaInfoReference! !

!CMGenericObject publicMethodsFor: 'boss'!

bossOutToFileNamed: theFileName

	| unFilename aBoss |

	theFileName isNil ifTrue: [ ^nil].
	unFilename := theFileName asFilename.

	[
		aBoss := BinaryObjectStorage onNew: (unFilename withEncoding: #binary) writeStream.
		aBoss nextPut: self
	]
		valueNowOrOnUnwindDo: 
	[
		aBoss isNil ifFalse: [ aBoss close]
	]! !

!CMGenericObject publicMethodsFor: 'catching accesses'!

doesNotUnderstand: theMessage

	| aSelector aMetaInfo aFeature |
self halt.
	theMessage isNil ifTrue: [ ^super doesNotUnderstand: theMessage].

	aSelector := theMessage selector.

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super doesNotUnderstand: theMessage].

	aFeature := aMetaInfo featureOrInheritedNamed: aSelector asString.
 
	aFeature isNil ifTrue: [ ^super doesNotUnderstand: theMessage].

	^aFeature getObjectFeatureValueTC: self! !

!CMGenericObject publicMethodsFor: 'initialize-release'!

forzeMetaInfo: theMetaInfo
	self propertyAt: self class metaInfoPropertyName put: theMetaInfo!

initWithMetaInfo: theMetaInfo
	self propertyAt: self class metaInfoPropertyName put: theMetaInfo!

initWithMetaInfo: theMetaInfo enumValue: theEnumValue
	self propertyAt: self class metaInfoPropertyName put: theMetaInfo.
	self propertyAt: self class enumValuePropertyName put: theEnumValue!

release
	super release! !

!CMGenericObject publicMethodsFor: 'metainfo attach-detach'!

attachToMetaInfoIn: theAttachMetaInfoCache model: theModel
	| aMetaInfo aFullyQualifedName anEnumFullyQualifedName anEnumValue |

	theAttachMetaInfoCache isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifFalse: [ ^self].
	
	aFullyQualifedName := self propertyAt: self class detachedMetaInfoPropertyName.
	(aFullyQualifedName isNil or: [ aFullyQualifedName isEmpty])  ifFalse: [
	
		aMetaInfo := theAttachMetaInfoCache at: aFullyQualifedName ifAbsent: [ nil].
		aMetaInfo isNil ifTrue: [
			aMetaInfo := theModel typeNamed: aFullyQualifedName.
			aMetaInfo isNil ifFalse: [
				theAttachMetaInfoCache	at: aFullyQualifedName put: aMetaInfo
			]	
		].
		aMetaInfo isNil ifFalse: [
			self propertyAt: self class metaInfoPropertyName put: aMetaInfo.
			self removeProperty: self class detachedMetaInfoPropertyName.
		]
	].

	anEnumFullyQualifedName := self propertyAt: self class detachedEnumValuePropertyName.
	(anEnumFullyQualifedName isNil or: [ anEnumFullyQualifedName isEmpty]) ifTrue:  [ ^self].

	anEnumValue := theAttachMetaInfoCache at: anEnumFullyQualifedName ifAbsent: [ nil].
	anEnumValue isNil ifTrue: [
		anEnumValue := theModel findFeatureNamed: anEnumFullyQualifedName.
		anEnumValue isNil ifFalse: [
			theAttachMetaInfoCache	at: anEnumFullyQualifedName put: anEnumValue
		]
	].
	anEnumValue isNil ifTrue: [ ^self].

	self propertyAt: self class enumValuePropertyName put: anEnumValue.
	self removeProperty: self class detachedEnumValuePropertyName.!

detachFromDomain!

detachFromMetaInfoIn: theDetachMetaInfoFullyQualifiedNamesCache
	| aMetaInfo aFullyQualifedName anEnumValue anEnumFullyQualifedName |
	theDetachMetaInfoFullyQualifiedNamesCache isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifFalse: [

		aFullyQualifedName := theDetachMetaInfoFullyQualifiedNamesCache at: aMetaInfo ifAbsent: [ nil].
		aFullyQualifedName isNil ifTrue: [
			aFullyQualifedName := aMetaInfo fullyQualifiedName.
			theDetachMetaInfoFullyQualifiedNamesCache	at: aMetaInfo put: aFullyQualifedName
		].

		self propertyAt: self class detachedMetaInfoPropertyName put: aFullyQualifedName.
		self removeProperty: self class metaInfoPropertyName.
	].

	anEnumValue := self enumValue.
	anEnumValue isNil ifTrue: [ ^self].

	anEnumFullyQualifedName := theDetachMetaInfoFullyQualifiedNamesCache at: anEnumValue ifAbsent: [ nil].
	anEnumFullyQualifedName isNil ifTrue: [
		anEnumFullyQualifedName := anEnumValue fullyQualifiedName.
		theDetachMetaInfoFullyQualifiedNamesCache	at: anEnumValue put: anEnumFullyQualifedName
	].

	self propertyAt: self class detachedEnumValuePropertyName put: anEnumFullyQualifedName.
	self removeProperty: self class enumValuePropertyName.!

recursiveAttachToMetaInfoIn: theAttachMetaInfoCache model: theModel
	| aMetaInfo  |

	theAttachMetaInfoCache isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifFalse: [ ^self].

	self attachToMetaInfoIn: theAttachMetaInfoCache model: theModel.
		
	self keys copy do: [:aKey |  | anObject  |
		aKey == #archivo ifFalse: [ 
			anObject := self propertyAt: aKey.
			(anObject isKindOf: self class) 
				ifTrue: [ anObject recursiveAttachToMetaInfoIn: theAttachMetaInfoCache model: theModel]
				ifFalse: [ 
					((anObject isKindOf: Collection) and: [ (anObject isKindOf: String) not]) ifTrue: [ 
						anObject do: [:anObj | 
							(anObj isKindOf: self class) ifTrue: [ 
								anObj recursiveAttachToMetaInfoIn: theAttachMetaInfoCache model: theModel
							]
						]
					]
			]
		]
	]!

recursiveAttachToMetaInfoInModel: theModel

	theModel isNil ifTrue: [ ^nil].

	^self recursiveAttachToMetaInfoIn: (IdentityDictionary new: 553)  model: theModel!

recursiveDetachFromMetaInfo

	^self recursiveDetachFromMetaInfoIn: (IdentityDictionary new: 553)!

recursiveDetachFromMetaInfoIn: theDetachMetaInfoFullyQualifiedNamesCache
	| aMetaInfo  anEnumValue |
	theDetachMetaInfoFullyQualifiedNamesCache isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	anEnumValue := self enumValue.
	(aMetaInfo isNil and: [ anEnumValue isNil]) ifTrue: [ ^self].

	self detachFromMetaInfoIn: theDetachMetaInfoFullyQualifiedNamesCache.
		
	self keys copy do: [:aKey |  | anObject  |
		aKey == #archivo ifFalse: [ 
			anObject := self propertyAt: aKey.
			(anObject isKindOf: self class) 
				ifTrue: [ anObject recursiveDetachFromMetaInfoIn: theDetachMetaInfoFullyQualifiedNamesCache]
				ifFalse: [ 
					((anObject isKindOf: Collection) and: [ (anObject isKindOf: String) not]) ifTrue: [ 
						anObject do: [:anObj | 
							(anObj isKindOf: self class) ifTrue: [ 
								anObj recursiveDetachFromMetaInfoIn: theDetachMetaInfoFullyQualifiedNamesCache
							]
						]
					]
				]
		]
	]! !

!CMGenericObject publicMethodsFor: 'navigation'!

browse

	^self browseWithDefinitionsHolderClass: nil!

browsePath

	^self browsePathWithDefinitionsHolderClass: nil!

browsePathBeDialog

	^self browsePathBeDialogWithDefinitionsHolderClass: nil!

browsePathBeDialogWithDefinitionsHolderClass: theDefinitionsHolderClass

	| aMetaInfo aModel aDefinitionsHolder aDefinitionsHolderClass aResult aBrowserClass |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aDefinitionsHolderClass := theDefinitionsHolderClass isNil 
		ifFalse: [ theDefinitionsHolderClass]
		ifTrue: [ aMetaInfo definitionsHolderClassForInstances].

	aDefinitionsHolderClass isNil ifTrue: [ aDefinitionsHolderClass := self preferredPreferencesClass preferredDefinitionsHolderClass].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := aDefinitionsHolderClass fromModel: aModel.

	aBrowserClass:= aDefinitionsHolder preferredApplicationBrowserClass.

	aResult := aBrowserClass
		openForObject: 			self 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 1;
				at:  METABrowser initialVerticalCanvasesProportionParameterSymbol put: 0;
				yourself)
		beDialog: true
		selectionOn: nil asValue.
	^aResult!

browsePathWithDefinitionsHolderClass: theDefinitionsHolderClass

	| aMetaInfo aModel aDefinitionsHolder aDefinitionsHolderClass aBrowserClass |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aDefinitionsHolderClass := theDefinitionsHolderClass isNil 
		ifFalse: [ theDefinitionsHolderClass]
		ifTrue: [ aMetaInfo model definitionsHolderClassForInstances].

	aDefinitionsHolderClass isNil ifTrue: [ aDefinitionsHolderClass := self preferredPreferencesClass preferredDefinitionsHolderClass].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := aDefinitionsHolderClass fromModel: aModel.

	aBrowserClass:= aDefinitionsHolder preferredPathFinderApplicationBrowserClass.

	aBrowserClass
		openForObject: 			self 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	nil!

browseWithDefinitionsHolderClass: theDefinitionsHolderClass

	| aMetaInfo aModel aDefinitionsHolder aDefinitionsHolderClass aBrowserClass |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aDefinitionsHolderClass := theDefinitionsHolderClass isNil 
		ifFalse: [ theDefinitionsHolderClass]
		ifTrue: [ aMetaInfo definitionsHolderClassForInstances].

	aDefinitionsHolderClass isNil ifTrue: [ aDefinitionsHolderClass := self preferredPreferencesClass preferredDefinitionsHolderClass].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := aDefinitionsHolderClass fromModel: aModel.

	aBrowserClass:= aDefinitionsHolder preferredApplicationBrowserClass.

	aBrowserClass
		openForObject: 			self 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters: nil!

metaClass

	self halt.
	^super metaClass!

metaIcon
	^nil!

metaNameSelector
	| aMetaInfo |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^#printString].
	^aMetaInfo metaNameSelectorForObject: self!

pathSelectors
	^nil! !

!CMGenericObject publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^self class preferredPreferencesClass! !

!CMGenericObject publicMethodsFor: 'printing'!

printOn: theStream

	| aMetaInfo aNameString anIDString |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self printOnDetached: theStream].

	theStream nextPutAll: ' { '; 
		nextPutAll: ''''; nextPutAll: aMetaInfo nlsName; nextPutAll: '''';
		nextPutAll: ' (';
		nextPutAll: aMetaInfo name;
		nextPutAll: ') '.

	aNameString := nil.
	Object errorSignal handle: [:anEx | ] do: [ aNameString := aMetaInfo getObjectNameValueNoDefault: self].
	aNameString isNil ifTrue: [ aNameString := '?N'].
	(aNameString isKindOf: String) ifFalse: [ aNameString := aNameString asString].

	anIDString := aMetaInfo getObjectIDValueNoDefault: self.
	anIDString isNil ifTrue: [ anIDString := '?I'].
	(anIDString isKindOf: String) ifFalse: [ anIDString := anIDString asString].

	theStream nextPutAll: ''''; nextPutAll: aNameString; nextPutAll: ''''; space;  nextPutAll: anIDString;  nextPutAll: ' } '; cr!

printOn: theStream printed: thePrinted

	| tooMany |
	
	tooMany := theStream position + self maxPrint.
	theStream nextPutAll: ' (CMGO ('.
(thePrinted includes: self) 
	ifTrue:  [ theStream nextPutAll: '#^'] 
	ifFalse: [ 
	self associationsDo: 
		[:element | 
		theStream position > tooMany ifTrue: [theStream nextPutAll: '...etc...)'. ^self].
		element key printOn: theStream.
		theStream nextPutAll: '->'.
		thePrinted add: self.
		(element value isKindOf: self class) 
			ifTrue: [ element value printOn: theStream printed: thePrinted]
			ifFalse: [ 
				((element value isKindOf: Collection) and: [ (element value isKindOf: String) not and:  [ (element value isKindOf: Symbol) not]])
					ifTrue: [ 
						element value do: [:aColElem |
							(aColElem isKindOf: self class) 
								ifTrue: [ aColElem value printOn: theStream printed: thePrinted]
								ifFalse: [ aColElem value printOn: theStream]
						]
					]
					ifFalse: [  element value printOn: theStream ]
			].
		theStream space]
].
	theStream nextPutAll: ' ))'!

printOnDetached: theStream

	| |

	theStream nextPutAll: ' { '; 
		nextPutAll: ''''; nextPutAll: (self propertyAt: self class detachedMetaInfoPropertyName) printString; nextPutAll: '''';
		nextPutAll: ' }'!

printOnWithNested: theStream

	| somePrinted |
	somePrinted := IdentitySet new: 13.
	self printOn: theStream printed: somePrinted! !

!CMGenericObject publicMethodsFor: 'TRF-change events'!

notifyChangeEvent: theFeatureName withNew: theFeatureValue withSaved: theSavedFeatureValue

	self changed: theFeatureName!

notifyDeletionChangeEvent

	self changed: #objectDisconnectedOfTree! !

!CMSystemPrimitiveBroker class publicMethodsFor: 'brokers'!

brokers
	
	brokers isNil ifTrue: [ self initBrokers].
	^brokers!

primitiveBrokerForType: theType

	| aFoundBroker aReengineredClassName aReengineredClass  aCustomBroker |
	theType isNil ifTrue: [ ^nil].
	
	aFoundBroker := self brokers detect: [:aBroker | aBroker name = theType name] ifNone: [ nil].
	aFoundBroker isNil ifFalse: [ ^aFoundBroker].

	aReengineredClassName := theType reengineredClassName.
	(aReengineredClassName isNil or: [ aReengineredClassName isEmpty]) ifTrue: [ ^nil].

	aFoundBroker := self brokers detect: [:aBroker | aBroker name = aReengineredClassName] ifNone: [ nil].
	aFoundBroker isNil ifFalse: [ ^aFoundBroker].

	aReengineredClass := Smalltalk at: aReengineredClassName ifAbsent: [ nil].
	aReengineredClass isNil ifTrue: [ ^nil].

	aCustomBroker := self new .
	aCustomBroker
		name: 							aReengineredClassName copy
		primitiveClass: 					aReengineredClass 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: aReengineredClass ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ aReengineredClass new]
		createFromStringBlock:  		[:anInitStr |  aReengineredClass readFrom: anInitStr readStream]
		createFromObjectBlock: 		[:aPrimitiveValue |   aReengineredClass newFrom: aPrimitiveValue]
		cloneBlock:  						[:anObject | anObject copy].

	^aCustomBroker! !

!CMSystemPrimitiveBroker class publicMethodsFor: 'brokers-initialization'!

buildBrokerForBoolean

	| aBroker |

	aBroker := self new .
	aBroker
		name: 									'Boolean' copy
		primitiveClass: 							Boolean 
		isTypeOfBlock: 							[:anObj |  anObj isKindOf: Boolean ]
		isNonEmptyBlock: 						[:anObject | true]
		createBlock:  							[ true]
		createFromStringBlock:  				[:anInitStr |  anInitStr trimBlanks asLowercase = true asString]
		createFromObjectBlock: 				[:aPrimitiveValue | aPrimitiveValue]
		cloneBlock:  								[:anObject | anObject ]
		specTypeBlock:							[ #Boolean]
		nameStringFromObjectBlock: 		[:anObject | anObject asString]
		widgetType:								nil
		formatString:								nil.

	^aBroker!

buildBrokerForDate

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Date' copy
		primitiveClass: 					Date 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Date ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ Date today ]
		createFromStringBlock:  		[:anInitStr |  Date readFrom: anInitStr readStream]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue copy]
		cloneBlock:  						[:anObject | anObject copy]
		specTypeBlock:					[ #Date]
		nameStringFromObjectBlock: [:anObject | anObject printString]
		widgetType:						#date
		formatString:						'mm/dd/yyyy' copy.
	^aBroker!

buildBrokerForImage

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Image' copy
		primitiveClass: 					Image 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Image ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ Image new ]
		createFromStringBlock:  		[:anInitStr |  Image readFrom: anInitStr readStream]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue copy]
		cloneBlock:  						[:anObject | anObject copy]
		specTypeBlock:					[ #Image]
		nameStringFromObjectBlock: [:anObject | anObject printString]
		widgetType:						#image
		formatString:						'' copy.
	^aBroker!

buildBrokerForInteger

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Integer' copy
		primitiveClass: 					Integer 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Integer ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ 0 ]
		createFromStringBlock:  		[:anInitStr |  Integer readFrom: anInitStr readStream]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue ]
		cloneBlock:  						[:anObject | anObject ]
		specTypeBlock:					[ #Number]
		nameStringFromObjectBlock: [:anObject | anObject printString]
		widgetType:						#number
		formatString:						nil.
	^aBroker!

buildBrokerForNumber

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Number' copy
		primitiveClass: 					Number 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Number ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ 0]
		createFromStringBlock:  		[:anInitStr |  Number readFrom: anInitStr readStream]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue copy]
		cloneBlock:  						[:anObject | anObject copy]
		specTypeBlock:					[ #Number]
		nameStringFromObjectBlock: [:anObject | anObject printString]
		widgetType:						#number
		formatString:						nil.

	^aBroker!

buildBrokerForObject

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Object' copy
		primitiveClass: 					Object 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Object ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ true]
		createFromStringBlock:  		[:anInitStr |  Object new]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue copy]
		cloneBlock:  						[:anObject | anObject copy ]
		specTypeBlock:					[ #String]
		nameStringFromObjectBlock: [:anObject | anObject printString firstLine: 80]
		widgetType:						#object
		formatString:						nil.

	^aBroker!

buildBrokerForSpanishDate

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'SpanishDate' copy
		primitiveClass: 					SpanishDate 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: SpanishDate ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ SpanishDate today ]
		createFromStringBlock:  		[:anInitStr |  SpanishDate readFrom: anInitStr readStream]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue copy]
		cloneBlock:  						[:anObject | anObject copy]
		specTypeBlock:					[ #Date]
		nameStringFromObjectBlock: [:anObject | anObject printString]
		widgetType:						#date
		formatString:						'dd/mm/yyyy' copy.
	^aBroker!

buildBrokerForString

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'String' copy
		primitiveClass: 					String 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: String ]
		isNonEmptyBlock: 				[:anObject | anObject trimBlanks isEmpty not]
		createBlock:  					[ String new]
		createFromStringBlock:  		[:anInitStr |  anInitStr asString copy]
		createFromObjectBlock: 		[:aPrimitiveValue |  aPrimitiveValue asString copy]
		cloneBlock:  						[:anObject | anObject asString copy]
		specTypeBlock:					[ #String]
		nameStringFromObjectBlock: [:anObject | anObject  asString firstLine: 80]
		widgetType:						#string
		formatString:						nil.
	^aBroker!

buildBrokerForSymbol

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Symbol' copy
		primitiveClass: 					Symbol 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Symbol ]
		isNonEmptyBlock: 				[:anObject | anObject trimBlanks isEmpty not]
		createBlock:  					[ #'' ]
		createFromStringBlock:  		[:anInitStr |  anInitStr trimBlanks asSymbol]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue]
		cloneBlock:  						[:anObject | anObject ]
		specTypeBlock:					[ #String]
		nameStringFromObjectBlock: [:anObject | anObject asString]
		widgetType:						#string
		formatString:						nil.
	^aBroker!

buildBrokerForText

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Text' copy
		primitiveClass: 					Text 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Text ]
		isNonEmptyBlock: 				[:anObject | anObject string trimBlanks isEmpty not]
		createBlock:  					[ Text new]
		createFromStringBlock:  		[:anInitStr |  anInitStr asText copy]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue asText copy]
		cloneBlock:  						[:anObject | anObject asText copy]
		specTypeBlock:					[ #Text]
		nameStringFromObjectBlock: [:anObject | (anObject string firstLine: 80) ]
		widgetType:						#string
		formatString:						nil.

	^aBroker!

buildBrokerForTime

	| aBroker |

	aBroker := self new .
	aBroker
		name: 							'Time' copy
		primitiveClass: 					Time 
		isTypeOfBlock: 					[:anObj |  anObj isKindOf: Time ]
		isNonEmptyBlock: 				[:anObject | true]
		createBlock:  					[ Time now]
		createFromStringBlock:  		[:anInitStr |  Time readFrom: anInitStr readStream]
		createFromObjectBlock: 		[:aPrimitiveValue |   aPrimitiveValue copy]
		cloneBlock:  						[:anObject | anObject copy]
		specTypeBlock:					[ #Time]
		nameStringFromObjectBlock: [:anObject | anObject printString]
		widgetType:						#time
		formatString:						'hh:mm:ss ampm' copy.

	^aBroker!

initBrokers
	
	"CMSystemPrimitiveBroker initBrokers"

	brokers := OrderedCollection new.
	
	brokers add: self buildBrokerForString.
	brokers add: self buildBrokerForInteger.
	brokers add: self buildBrokerForBoolean.
	brokers add: self buildBrokerForDate.
	brokers add: self buildBrokerForTime.
	brokers add: self buildBrokerForSymbol.
	brokers add: self buildBrokerForText.
	brokers add: self buildBrokerForNumber.
	brokers add: self buildBrokerForObject.
	brokers add: self buildBrokerForSpanishDate.
	brokers add: self buildBrokerForImage.!

resetBrokers
	
	"CMSystemPrimitiveBroker resetBrokers"

	brokers := nil! !

!CMSystemPrimitiveBroker class publicMethodsFor: 'nls'!

nlsAppName
	^'SystemPrimitiveBroker' copy!

nlsGroupName
	^'PrimitiveBooleanValues' copy!

nlsGroupNameBooleanValues
	^'PrimitiveBooleanValues' copy!

nlsItemNameBooleanFalse
	^'false' copy!

nlsItemNameBooleanTrue
	^'true' copy! !

!CMSystemPrimitiveBroker publicMethodsFor: 'accessing'!

formatString
	^formatString!

name
	^name!

primitiveClass
	^self primitiveClass!

widgetType
	^widgetType! !

!CMSystemPrimitiveBroker publicMethodsFor: 'initialize-release'!

name: theName
	primitiveClass: thePrimitiveClass
	isTypeOfBlock:  theIsTypeOfBlock
	isNonEmptyBlock: theIsNonEmptyBlock
	createBlock:  theCreateBlock
	createFromStringBlock:  theCreateFromStringBlock
	createFromObjectBlock:  theCreateFromObjectBlock
	cloneBlock:  theCloneBlock
	specTypeBlock:  theSpecTypeBlock
	nameStringFromObjectBlock: theNameStringFromObjectBlock
	widgetType: theWidgetType
	formatString: theFormatString

	name := theName.
	primitiveClass := thePrimitiveClass.
	isTypeOfBlock :=  theIsTypeOfBlock.
	isNonEmptyBlock := theIsNonEmptyBlock.
	createBlock :=  theCreateBlock.
	createFromStringBlock :=  theCreateFromStringBlock.
	createFromObjectBlock :=  theCreateFromObjectBlock.
	cloneBlock :=  theCloneBlock.
	specTypeBlock :=  theSpecTypeBlock.
	nameStringFromObjectBlock := theNameStringFromObjectBlock.
	widgetType := theWidgetType.
	formatString := theFormatString! !

!CMSystemPrimitiveBroker publicMethodsFor: 'primitive access'!

clone: theObject

	| aNewObject |
	theObject isNil  ifTrue: [ ^nil].

	(theObject isKindOf: primitiveClass) ifFalse: [ ^nil].

	aNewObject := nil.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aNewObject := cloneBlock value: theObject].

	^aNewObject!

create

	| aNewObject |

	aNewObject := nil.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aNewObject := createBlock value].

	^aNewObject!

createFromObject: theObject

	| aNewObject |
	theObject isNil  ifTrue: [ ^nil].

	(theObject isKindOf: primitiveClass) ifFalse: [ ^nil].

	aNewObject := nil.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aNewObject := createFromObjectBlock value: theObject].

	^aNewObject!

createFromString: theString

	| aNewObject |
	theString isNil  ifTrue: [ ^nil].
 
	aNewObject := nil.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aNewObject := createFromStringBlock value: theString].

	^aNewObject!

isNonEmpty: theObject

	| aResult |
	theObject isNil  ifTrue: [ ^false].

	(theObject isKindOf: primitiveClass) ifFalse: [ ^false].

	aResult := false.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aResult := isNonEmptyBlock value: theObject].

	^aResult!

isTypeOf: theObject

	| aResult |
	theObject isNil  ifTrue: [ ^false].

	(theObject isKindOf: primitiveClass) ifFalse: [ ^false].

	aResult := nil.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aResult := isTypeOfBlock value: theObject].

	^aResult!

nameStringFromObject: theObject

	| aNewObject |
	theObject isNil  ifTrue: [ ^nil].

	(theObject isKindOf: primitiveClass) ifFalse: [ ^nil].

	aNewObject := nil.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aNewObject := nameStringFromObjectBlock value: theObject].

	^aNewObject!

specType

	| aNewObject |

	aNewObject := nil.

	Object errorSignal 
		handle: [:anEx | ]
		do: [  aNewObject := specTypeBlock value].

	^aNewObject! !

!CMSystemPrimitiveBroker publicMethodsFor: 'primitive types'!

isIDCompatible
	^self isPrimitiveInteger or: [ self isPrimitiveNumber or: [ self isPrimitiveString]]!

isPrimitiveBoolean
	^primitiveClass == Boolean!

isPrimitiveDate
	^primitiveClass == Date or: [ primitiveClass == SpanishDate]!

isPrimitiveImage
	^primitiveClass == Image or: [ primitiveClass inheritsFrom: Image]!

isPrimitiveInteger
	^primitiveClass == Integer!

isPrimitiveNumber
	^primitiveClass == Number!

isPrimitiveString
	^primitiveClass == String!

isPrimitiveSymbol
	^primitiveClass == Symbol!

isPrimitiveText
	^primitiveClass == Text!

isPrimitiveTime
	^primitiveClass == Time! !

!CMSystemPrimitiveBroker publicMethodsFor: 'printing'!

printOn: theStream
	name isNil ifTrue: [ ^super printOn: theStream].
	theStream nextPutAll: ' (PrimBroker '; nextPutAll: name; nextPutAll: ') '! !

!CMTransaction class publicMethodsFor: 'current'!

mustPropagateChangeEventsOnCommit: theBool

	^self transactionSignal raiseRequestWith: (Array with: #mustPropagateChangeEventsOnCommit with: theBool)! !

!CMTransaction class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences!

preferredTransactionNesterClass
	^self preferredPreferencesClass preferredTransactionNesterClass! !

!CMTransaction publicMethodsFor: 'accessing'!

mustPropagateChangeEventsOnCommit

	^mustPropagateChangeEventsOnCommit == true! !

!CMTransaction publicMethodsFor: 'change events'!

propagateChangeEvents

	| someChangedEntities |

	someChangedEntities := IdentitySet new: 13.

	self propagateChangeEventsChangeEntitiesInto: someChangedEntities.

	^someChangedEntities!

propagateChangeEventsChangeEntitiesInto: theChangedEntities

	self do: [:aLinker |  aLinker propagateChangeEventsChangeEntitiesInto: theChangedEntities].! !

!CMTransaction publicMethodsFor: 'executing'!

completeTransaction

	| someChangedEntities |

	nester transaction isNil 
		ifTrue: [  
			someChangedEntities := self propagateChangeEvents.
			Signal noHandlerSignal 
				handle: [:anException | anException returnWith: nil]
				do: [ Transaction transactionCommitedSignal raiseRequestWith: someChangedEntities].

			[  self releaseAndChain ] valueOnUnwindDo: [
				self error: 'Incomplete release of transaction'
			]
		]
		ifFalse: [ 
			self mustPropagateChangeEventsOnCommit ifTrue: [ 
				self propagateChangeEvents.
			]
		]!

handleExceptionMustPropagateChangeEventsOnCommit: theException

	| unWhat unWhich |

	unWhat 	:=  theException parameter at: 1.
	unWhich :=  theException parameter at: 2.	

	unWhat == #mustPropagateChangeEventsOnCommit ifFalse: [ ^self].

	unWhich isNil ifTrue: [ ^self].

	mustPropagateChangeEventsOnCommit := unWhich == true!

handleExceptionOther: theException

	| unWhat  |

	unWhat 	:=  theException parameter at: 1.

	unWhat == #mustPropagateChangeEventsOnCommit ifTrue: [ ^self handleExceptionMustPropagateChangeEventsOnCommit: theException].! !

!CMTransaction publicMethodsFor: 'initialization'!

initialize

	self class allowDebug == true ifFalse: [ self senderMustBeSelfClassOrSuperClassOrSubClass].
	super initialize.

	mustPropagateChangeEventsOnCommit := false!

mustPropagateChangeEventsOnCommit: theBool

	mustPropagateChangeEventsOnCommit := theBool == true! !

!CMTransactionNester publicMethodsFor: 'change events'!

propagateChangeEventsChangeEntitiesInto: theChangedEntities
	subTransaction isNil ifFalse: [  subTransaction propagateChangeEventsChangeEntitiesInto: theChangedEntities]! !

!CMTypeDependency class publicMethodsFor: 'instance creation'!

forType: theType feature: theFeature path: thePath

	| aDependency |

	theType isNil ifTrue: [ ^nil].
	theFeature isNil ifTrue: [ ^nil].
	(thePath isNil or: [ thePath isEmpty]) ifTrue: [ ^nil].

	aDependency := self new.
	aDependency buildForType: theType feature: theFeature path: thePath.
	^aDependency! !

!CMTypeDependency class publicMethodsFor: 'navigation'!

defaultBrowserParameters
	^self browserParametersForModelEditor!

defaultChooserParameters
	^self browserParametersForElementChooser!

defaultDefinitionsHolder
	^CODEMModelDefinitionsHolder forModelEditor "forModelEditorWithCompositeChildSpec"!

metaSelectors

	^self modelEditorMETASelectors!

modelEditorMETAPerspectives

	^METAChildSpec autoMETAPerspectivesFrom: self!

modelEditorMETASelectors

	"METAChildSpecAutoViewEditor openOn: CMTypeDependency selector: #modelEditorMETASelectors target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 5)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DisplayName';
			basicSelector: #displayName;
			type: #String;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DisplayName';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Path';
			basicSelector: #path;
			type: #String;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Path';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Type';
			basicSelector: #type;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Type';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEType;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Feature';
			basicSelector: #feature;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Feature';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEStructuralFeature;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'FirstObservers';
			basicSelector: #firstObservers;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'FirstObservers';
			displaySelector: #displayName;
			canShowInTree: true;
			componentsClassName: #CMTypeObserver;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			yourself);
		yourself!

modelEditorPathSelectors

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #()! !

!CMTypeDependency publicMethodsFor: 'accessing'!

feature
	^feature!

firstObservers
	^firstObservers!

path
	^path!

type
	^type! !

!CMTypeDependency publicMethodsFor: 'building'!

addFirstObserver: theObserver
	theObserver isNil ifTrue: [ ^self].

	firstObservers isNil ifTrue: [ firstObservers := OrderedCollection new: 2].
	firstObservers add: theObserver!

buildForType: theType feature: theFeature  path: thePath
	
	theType isNil ifTrue: [ ^nil].
	theFeature isNil ifTrue: [ ^nil].
	(thePath isNil or: [ thePath isEmpty]) ifTrue: [ ^nil].

	type := theType.
	feature := theFeature.
	path := thePath.! !

!CMTypeDependency publicMethodsFor: 'initialize-release'!

release
	self changed: #objectDisconnectedOfTree.

	firstObservers isNil ifFalse: [ 
		firstObservers do: [:anObserver | anObserver release]
	].
	firstObservers := nil.

	feature isNil ifFalse: [ feature dependenciesRemove: self].

	type := nil.
	feature := nil.
	path := nil! !

!CMTypeDependency publicMethodsFor: 'navigation'!

browse
	^CODEMModelGenericBrowser
		openForObject: 			self 
		definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:	self defaultBrowserParameters!

browsePath

	^CODEMModelPathFinderGenericBrowser
		openForObject: 			self 
		definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:	self defaultBrowserParameters!

defaultBrowserParameters
	^self class defaultBrowserParameters!

defaultChooserParameters
	^self class defaultChooserParameters!

defaultDefinitionsHolder
	^self class defaultDefinitionsHolder!

displayName
	| aStream |
	aStream := WriteStream on: (String new: 32).

	aStream nextPutAll: ( type isNil ifTrue: [ '?T'] ifFalse: [  type name]) , ' ', 
		 ( feature isNil ifTrue: [ '?F'] ifFalse: [  feature name]).
	^aStream contents!

displaySelector
	^#displayName!

metaClass

	^self class!

metaEditorClassLabel

	^self classLabelForMETAEditor!

metaNameSelector
	^#displayName!

metaSelectorsSelector
	^#metaSelectors

"*VIPVersion 15-7-97 | 8:30:48 pm 'ACV'*"! !

!CMTypeDependency publicMethodsFor: 'printing'!

printOn: elStream
	elStream nextPutAll: (self class printString , ' ', ( type isNil ifTrue: [ '?T'] ifFalse: [  type name]) , ' ', 
		 ( feature isNil ifTrue: [ '?F'] ifFalse: [  feature name])), ' )'! !

!CMTypeDependency publicMethodsFor: 'TRF-dependency'!

notifyChangeForObject: theObject value: theNewObject

	theObject isNil ifTrue: [ ^self].
	feature isNil ifTrue: [ ^self].

	feature notifyChangeForObject: theObject value: theNewObject!

 updateFromObserver: theObserver object: theObject value: theNewObject

	| |
	theObject isNil ifTrue: [ ^self].

	^self notifyChangeForObject: theObject value: theNewObject! !

!CMTypeObserver class publicMethodsFor: 'instance creation'!

after: thePreviousTypeObserver onDependency: theDependency forType: theType feature: theFeature path: thePath


	| anObserver |

	theDependency isNil ifTrue: [ ^nil].
	theType isNil ifTrue: [ ^nil].
	theFeature isNil ifTrue: [ ^nil].
 
	anObserver := self new.
	anObserver buildAfter: thePreviousTypeObserver onDependency: theDependency forType: theType feature: theFeature path: thePath.
	^anObserver! !

!CMTypeObserver class publicMethodsFor: 'navigation'!

defaultBrowserParameters
	^self browserParametersForModelEditor!

defaultChooserParameters
	^self browserParametersForElementChooser!

defaultDefinitionsHolder
	^CODEMModelDefinitionsHolder forModelEditor "forModelEditorWithCompositeChildSpec"!

metaSelectors

	^self modelEditorMETASelectors!

modelEditorMETAPerspectives

	^METAChildSpec autoMETAPerspectivesFrom: self!

modelEditorMETASelectors

	"METAChildSpecAutoViewEditor openOn: CMTypeObserver selector: #modelEditorMETASelectors target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 8)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DisplayName';
			basicSelector: #displayName;
			type: #String;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DisplayName';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Path';
			basicSelector: #path;
			type: #String;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Path';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Index';
			basicSelector: #index;
			type: #Number;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Index';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ObservedType';
			basicSelector: #observedType;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ObservedType';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEType;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ObservedFeature';
			basicSelector: #observedFeature;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ObservedFeature';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEStructuralFeature;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Dependency';
			basicSelector: #dependency;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Dependency';
			displaySelector: #displayName;
			canShowInTree: true;
			objectClassName: #CMTypeDependency;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'PreviousObserver';
			basicSelector: #previousObserver;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'PreviousObserver';
			displaySelector: #displayName;
			canShowInTree: true;
			objectClassName: #CMTypeObserver;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'NextObservers';
			basicSelector: #nextObservers;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'NextObservers';
			displaySelector: #displayName;
			canShowInTree: true;
			componentsClassName: #CMTypeObserver;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			yourself);
		yourself!

modelEditorPathSelectors

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #()! !

!CMTypeObserver publicMethodsFor: 'accessing'!

dependency
	^dependency!

index
	^index!

nextObservers
	^nextObservers!

observedFeature
	^observedFeature!

observedType
	^observedType!

path
	^path!

previousObserver
	^previousObserver! !

!CMTypeObserver publicMethodsFor: 'building'!

addNextObserver: theObserver
	theObserver isNil ifTrue: [ ^self].

	nextObservers isNil ifTrue: [ nextObservers := OrderedCollection new: 2].
	nextObservers add: theObserver!

buildAfter: thePreviousTypeObserver onDependency: theDependency forType: theType feature: theFeature path: thePath


	theDependency isNil ifTrue: [ ^nil].
	theType isNil ifTrue: [ ^nil].
	theFeature isNil ifTrue: [ ^nil].

	dependency := theDependency.
	observedType := theType.
	observedFeature := theFeature.
	path := thePath.

	thePreviousTypeObserver isNil
		ifTrue: [ 
			index := 1.
			theDependency addFirstObserver: self
		]
		ifFalse: [ 
			index := thePreviousTypeObserver index + 1.
			previousObserver := thePreviousTypeObserver.
			thePreviousTypeObserver addNextObserver: self
		].

	theType isNil ifFalse: [ theType attachObserver: self ].! !

!CMTypeObserver publicMethodsFor: 'initialize-release'!

release
	self changed: #objectDisconnectedOfTree.

	dependency := nil.

	observedType isNil ifFalse: [ 
		observedType detachObserver: self
	].

	observedType := nil.
	observedFeature := nil.

	nextObservers isNil ifFalse: [ 
		nextObservers do: [:anObserver | anObserver release]
	].
	nextObservers := nil.
	previousObserver := nil.
	path := nil! !

!CMTypeObserver publicMethodsFor: 'navigation'!

browse
	^CODEMModelGenericBrowser
		openForObject: 			self 
		definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:	self defaultBrowserParameters!

browsePath

	^CODEMModelPathFinderGenericBrowser
		openForObject: 			self 
		definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:	self defaultBrowserParameters!

defaultBrowserParameters
	^self class defaultBrowserParameters!

defaultChooserParameters
	^self class defaultChooserParameters!

defaultDefinitionsHolder
	^self class defaultDefinitionsHolder!

displayName
	| aStream |
	aStream := WriteStream on: (String new: 32).
	aStream nextPutAll: ( observedType isNil ifTrue: [ '?T'] ifFalse: [  observedType name]) ; space;
		nextPutAll: ( observedFeature isNil ifTrue: [ '?F'] ifFalse: [  observedFeature name]); space;
		nextPutAll: ( index isNil ifTrue: [ '?i'] ifFalse: [  index printString]); space;
		nextPutAll: ( path isNil ifTrue: [ '?p'] ifFalse: [  path asArray printString]).
	^aStream contents!

metaClass

	^self class!

metaEditorClassLabel

	^self classLabelForMETAEditor!

metaNameSelector
	^#displayName!

metaSelectorsSelector
	^#metaSelectors

"*VIPVersion 15-7-97 | 8:30:48 pm 'ACV'*"! !

!CMTypeObserver publicMethodsFor: 'printing'!

printOn: elStream
	elStream nextPutAll: self class printString ; space;
		nextPutAll: ( observedType isNil ifTrue: [ '?T'] ifFalse: [  observedType name]) ; space;
		nextPutAll: ( observedFeature isNil ifTrue: [ '?F'] ifFalse: [  observedFeature name]); space;
		nextPutAll: ( index isNil ifTrue: [ '?i'] ifFalse: [  index printString]); space;
		nextPutAll: ( path isNil ifTrue: [ '?p'] ifFalse: [  path asArray printString]); 
		nextPutAll: ' )'! !

!CMTypeObserver publicMethodsFor: 'TRF-dependency'!

backTraverseAndUpdateFromObject: theObject value: theNewObject

	| anInverseFeature aRelatedValue someObjects |
	theObject isNil ifTrue: [ ^self].

	dependency isNil ifTrue: [ ^self].

	observedFeature isNil ifTrue:  [ ^self].
	
	observedFeature isRelationship ifFalse: [ ^self].

	anInverseFeature := observedFeature inverse.
	anInverseFeature isNil ifTrue:  [ ^self].

	aRelatedValue := anInverseFeature getObjectFeatureValueTC: theObject.
	aRelatedValue isNil ifTrue: [ ^self].

	anInverseFeature isMultiplicityMany
		ifTrue: [ 
			someObjects := aRelatedValue.
			someObjects do: [:anObject | 
				 dependency updateFromObserver: self object: anObject  value: theNewObject
			]
		]
		ifFalse: [ dependency updateFromObserver: self object: aRelatedValue value: theNewObject]!

 update: theChangedFeature object: theObject value: theNewObject 

	theChangedFeature isNil ifTrue: [ ^self].
	theObject isNil ifTrue: [ ^self].

	dependency isNil ifTrue: [ ^self].
	observedFeature isNil ifTrue:  [ ^self].

	theChangedFeature == observedFeature ifFalse: [ ^self].

	previousObserver isNil
		ifTrue: [ dependency updateFromObserver: self object: theObject value: theNewObject.]
		ifFalse: [ 	previousObserver backTraverseAndUpdateFromObject: theObject value: theNewObject]! !

!CODEAttribute publicMethodsFor: 'TRF-change events'!

prChangeEventsFromLinker: theLinker

	| anObject aSaved aMetaInfo |

	theLinker isNil ifTrue: [ ^self].

	self isMultiplicityNone 	ifTrue: [ ^self].

	anObject := theLinker oneObject.
	anObject isNil ifTrue: [ ^self].

	aSaved := theLinker oneSaved.

	aMetaInfo := anObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo propagateChangeEventsFromObject: anObject feature: self saved: aSaved.!

propagateChangeEventsFromLinker: theLinker

	theLinker isNil ifTrue: [ ^nil].

	^self prChangeEventsFromLinker: theLinker! !

!CODEAttribute publicMethodsFor: 'TRF-deletion'!

disconnectForDelete: theObject

	| anExistingObject aResult |

	self isMultiplicityNone 	ifTrue: [ ^nil].

	self computationKind = self class computationKindAlways ifTrue: [ ^nil].

	anExistingObject := self getObjectAttributeValue: theObject.
	 anExistingObject isNil ifTrue: [ ^nil].

	self isMultiplicityMany ifTrue: [
		anExistingObject isEmpty ifTrue: [ ^nil]
	].

	self isMultiplicityMany 
		ifTrue: [ 
			anExistingObject do: [:anObject |
				 aResult := CMGenericLinkMaker unlinkerFrom: theObject to: anObject metaInfo: self
			]	
		]
		ifFalse: [ aResult := CMGenericLinkMaker unlinkerFrom: theObject to: anExistingObject metaInfo: self].

	^aResult!

disconnectForDeleteTC: theObject
	(self objectInstanceHasFeature: theObject)   ifFalse: [ ^nil].

	^self disconnectForDelete: theObject! !

!CODEAttribute publicMethodsFor: 'TRF-dependency'!

xbuildDependency

	super buildDependency.
self name = 'direccionOficial' ifTrue: [ self halt].
	self computationKind = self class computationKindAlways ifFalse: [ ^self].

	self buildDependencyWithExpression: self valueConstraint.! !

!CODEAttribute publicMethodsFor: 'TRF-derivations'!

canEvaluatePathOnInstances: theDerivationPath 
	
	| aValueType |

	theDerivationPath isNil ifTrue: [ ^true].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^false].

	theDerivationPath isEmpty ifTrue: [ ^true].

	(aValueType isPrimitive or: [ aValueType isEnumeration]) ifTrue: [ ^false].

	^aValueType canEvaluatePathOnInstances: theDerivationPath!

object: theObject derive: theDerivationPath clone: theClone
	
	| anObject aValueType aMetaInfo |
	theObject isNil ifTrue: [ ^nil].
	theDerivationPath isNil ifTrue: [ ^nil].

	self isMultiplicityMany ifTrue: [ 
		^self object: theObject deriveMany: theDerivationPath clone: theClone
	].

	anObject := self getObjectFeatureValueTC: theObject.
	anObject isNil ifTrue: [ ^nil].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^nil].

	(aValueType isTypeOfObjectInstance: anObject) ifFalse: [ ^nil].

	theDerivationPath isEmpty ifTrue: [ 
		^Array with: (theClone == true
			ifFalse: [ anObject] 
			ifTrue: [ aValueType cloneObject: anObject])
	].

	(aValueType isPrimitive or: [ aValueType isEnumeration]) ifTrue: [ ^nil "Path should have been empty upon reaching a primitive"].

	aMetaInfo := anObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	^aMetaInfo object: anObject derive: theDerivationPath clone: theClone!

object: theObject deriveMany: theDerivationPath clone: theClone
	
	|  someObjects someAllDerived aValueType aNonVirtualValueType |
	theObject isNil ifTrue: [ ^nil].
	theDerivationPath isNil ifTrue: [ ^nil].

	self isMultiplicityMany ifFalse: [ ^nil].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^nil].

	aNonVirtualValueType := aValueType nonVirtualType.
	aNonVirtualValueType isNil ifTrue: [ ^nil].

	(theDerivationPath isEmpty not and: [
		aValueType isPrimitive or: [ aValueType isEnumeration or: [ 
			aNonVirtualValueType isPrimitive or: [ aNonVirtualValueType isEnumeration]]]]) ifTrue: [ ^nil].

	someObjects := self getObjectFeatureValueTC: theObject.
	(someObjects isNil or: [ someObjects isEmpty]) ifTrue: [ ^nil].


	someAllDerived := OrderedCollection new: someObjects size * 3.

	someObjects do: [:anObject | | anObjectMetaInfo someDerived |
		theDerivationPath isEmpty 
			ifTrue: [ 
				someAllDerived add: (theClone == true
					ifFalse: [ anObject] 
					ifTrue: [ aValueType cloneObject: anObject])
			]
			ifFalse: [ 
				anObjectMetaInfo := anObject metaInfo.
				anObjectMetaInfo isNil ifFalse: [ 
					someDerived := anObjectMetaInfo object: anObject derive: theDerivationPath clone: theClone.
					(someDerived isNil not and: [ someDerived isEmpty not ]) ifTrue: [ 
						someAllDerived addAll: someDerived 
					]
				]
			]
	].

	^someAllDerived! !

!CODEAttribute publicMethodsFor: 'TRF-dynamic metainfo'!

evaluateValueConstraintObject: theObject

	| aConstraint aMetaInfo aComputedResult aExpression aResult |

	theObject isNil ifTrue: [ ^false].

	aConstraint := self valueConstraint.
	(aConstraint isNil or: [ aConstraint isEmpty]) ifTrue: [^false].

	aExpression := 'calc clonenot ' , aConstraint.

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^false].

	aComputedResult := aMetaInfo computeDerivedValueFrom: theObject calcExpression: aExpression.

	aResult := aComputedResult isNil 
		ifTrue: [ false] 
		ifFalse: [ 
			(aComputedResult isKindOf: Collection) 
				ifFalse: [ aComputedResult == true] 
				ifTrue: [  
					(aComputedResult isKindOf: String) 
						ifTrue: [ aComputedResult = true printString]  
						ifFalse: [ 
							aComputedResult isEmpty ifTrue: [ false] ifFalse: [ true]]]].
	^aResult!

requiredFeaturesTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes

	| aValueType somePrefixes |
	theRequiredFeaturesType isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].

	self computationKind = self class computationKindInitializedInConstructor ifFalse: [ ^self].


	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^self].
	
	somePrefixes := thePrefixes , (Array with: self).

	aValueType requiredFeaturesTypeMetaInfo: theRequiredFeaturesType  prefixes: somePrefixes!

validateRequiredFeaturesObject: theRequiredFeaturesObject

	| aConstraint aResult aValueType aValue aNonVirtualValueType |
	theRequiredFeaturesObject isNil ifTrue: [ ^nil].

	aConstraint := self valueConstraint.
	(aConstraint isNil or: [ aConstraint isEmpty]) ifFalse: [ 
		aResult := self evaluateValueConstraintObject: theRequiredFeaturesObject.
		aResult ifFalse: [ 	^Array with: self with: aConstraint ]
	].

	self minMult = self class minMultOptional ifTrue: [ ^nil].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^nil].

	aNonVirtualValueType := aValueType nonVirtualType.
	aNonVirtualValueType isNil ifTrue: [ ^nil].
		
	aValue := self getObjectFeatureValueTC: theRequiredFeaturesObject.
 
	^(aValueType isPrimitive or: [ aValueType isEnumeration or: [
		aNonVirtualValueType isPrimitive or: [ aNonVirtualValueType isEnumeration]]]) 
		ifTrue: [ 
			(aValueType isNonEmptyPrimitiveOrValidEnumerationInstance: aValue) 
				ifTrue: [ nil]
				ifFalse: [  Array with: self with: 'Es Requerido']
		]
		ifFalse: [ 
			(aValue isNil not and: [ aValueType isTypeOfObjectInstance: aValue])
				ifTrue: [ nil]
				ifFalse: [  Array with: self with: 'Es Requerido']
		]! !

!CODEAttribute publicMethodsFor: 'TRF-initialization'!

hasInitialization

	| aInitExpression someStrings  |

	aInitExpression := self initializationExpression.
	(aInitExpression isNil or: [ aInitExpression isEmpty]) ifTrue: [ ^false].

	someStrings := aInitExpression asArrayOfSubstrings.
	someStrings  size < 3 ifTrue: [ ^false].
	^true!

initializeNewObjectIDAttributes: theNewObject inHome: theHomeCMGO

	| aHomeMetaInfo aValueType aNonVirtualType anAttributeValue anAttributeValueMetaInfo aAttributeHomeIdCounter aCurrentCounter aNextCounter aNewObjectMetaInfo aNextCounterValue someValues |


	theNewObject isNil ifTrue: [ ^self].
	theHomeCMGO isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].

	self computationKind = self class computationKindInitializedFromHomeIdCounter ifFalse: [ ^nil].

	aNewObjectMetaInfo := theNewObject metaInfo.
	aNewObjectMetaInfo isNil ifTrue: [ ^self].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^self].

	aNonVirtualType := aValueType nonVirtualType.
	aNonVirtualType isNil  ifTrue: [ ^self].

	self isIDAttribute ifFalse: [ 
		aNonVirtualType isPrimitive not ifTrue:  [
			anAttributeValue := self getObjectAttributeValue: theNewObject.
			anAttributeValue isNil ifFalse: [  
				self isMultiplicityMany 
					ifFalse: [ 
						anAttributeValueMetaInfo := anAttributeValue metaInfo.
						anAttributeValueMetaInfo isNil ifFalse: [  
							anAttributeValueMetaInfo  initializeNewObjectIDAttributes: anAttributeValue inHome: theHomeCMGO.
						]
					]
					ifTrue: [ 
						someValues := anAttributeValue.		
						someValues do: [:aValue | | anAValueMetaInfo |			
							anAValueMetaInfo := aValue metaInfo.
							anAValueMetaInfo isNil ifFalse: [  
								anAValueMetaInfo  initializeNewObjectIDAttributes: aValue inHome: theHomeCMGO
							]
						]
					]
			]
		].
		^self
	].

	aNonVirtualType isIDCompatible  ifFalse: [ ^self].
	
	aHomeMetaInfo := theHomeCMGO metaInfo.
	aHomeMetaInfo isNil ifTrue: [ ^self].

	aAttributeHomeIdCounter := aHomeMetaInfo attributeOrInheritedNamed:  self class homeIDCounterCMGODomainAttributeName.
	aAttributeHomeIdCounter isNil ifTrue: [ ^self].

	aCurrentCounter := aAttributeHomeIdCounter getObjectAttributeValue: theHomeCMGO.
	aCurrentCounter isNil ifTrue: [ ^self].

	aNextCounter := aCurrentCounter + 1.

	aAttributeHomeIdCounter object: theHomeCMGO setTC: aNextCounter.
	
	aNextCounterValue := aNonVirtualType isPrimitiveString ifTrue: [  aNextCounter printString] ifFalse: [ aNextCounter].

	self object: theNewObject setTC: aNextCounterValue.!

initializeNewObjectRequiredFeature: theNewObject
	withRequiredFeaturesObject: theRequiredFeaturesObject
	prefixes: thePrefixes
	
	| aValueType aNewObject |
	theNewObject isNil ifTrue: [ ^self].

	self isIDAttribute ifTrue: [ ^self].
	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].

	self computationKind = self class computationKindInitializedInConstructor ifFalse: [ ^self].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^self].

	aNewObject := aValueType createObjectWithRequiredFeaturesObject: theRequiredFeaturesObject
		prefixes: thePrefixes, (Array with: self).

	self isMultiplicityMany
		ifFalse: [ 	self object: theNewObject setTC: aNewObject]
		ifTrue: [ self object: theNewObject addTC: aNewObject]!

initializeNewObjectRequiredFeatureFromInitializationExpression: theNewObject
	
	| aInitExpression someStrings aWhen |

	theNewObject isNil ifTrue: [ ^self].

	aInitExpression := self initializationExpression.
	(aInitExpression isNil or: [ aInitExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := aInitExpression asArrayOfSubstrings.
	someStrings isEmpty ifTrue: [ ^nil].

	aWhen := someStrings first.
	aWhen asSymbol = self class initExpressionWhenConnectedToContainer ifTrue: [ ^nil].

	^nil! !

!CODEAttribute publicMethodsFor: 'TRF-initialization-connect'!

initialize: theNewObject afterConnectionTo: theObject

	| aRelatedObjects  otherRelatedMetaInfo aValueType aNonVirtualValueType aThereWasInitialization aThereWasInitializationInRelated |

	theNewObject isNil ifTrue: [ ^false].
	theObject isNil ifTrue: [ ^false].

	self computationKind = self class computationKindAlways ifTrue: [ ^false].

	aThereWasInitialization := self initializeConnectedObjectComputedFeature: theNewObject afterConnectionTo: theObject.

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^aThereWasInitialization].

	aNonVirtualValueType := aValueType nonVirtualType.
	aNonVirtualValueType isNil ifTrue: [ ^aThereWasInitialization].

	(aNonVirtualValueType isPrimitive or: [ aNonVirtualValueType isEnumeration]) ifTrue: [ ^aThereWasInitialization].

	aRelatedObjects := self getObjectFeatureValueTC: theNewObject.
	self isMultiplicityMany
		ifTrue: [ 
			aRelatedObjects isEmpty ifFalse: [ 
				aRelatedObjects do: [:aRelatedObject | | aRelatedMetaInfo aThereWasInitInRel |
					aRelatedMetaInfo := aRelatedObject metaInfo.
					aRelatedMetaInfo isNil ifFalse: [ 
						aThereWasInitInRel := aRelatedMetaInfo initialize: aRelatedObject afterConnectionTo: theNewObject.
						aThereWasInitialization  := aThereWasInitialization or: [ aThereWasInitInRel].
					]
				]
			]
		]
		ifFalse: [ 
			aRelatedObjects isNil ifFalse: [ 
				otherRelatedMetaInfo := aRelatedObjects metaInfo.
				otherRelatedMetaInfo isNil ifFalse: [ 
					aThereWasInitializationInRelated := otherRelatedMetaInfo initialize: aRelatedObjects afterConnectionTo: theNewObject.
					aThereWasInitialization  := aThereWasInitialization or: [ aThereWasInitializationInRelated].
				]
			]
		].

	^aThereWasInitialization!

initialize: theNewObject afterConnectionTo: theObject linkBlock: theBlock

	|  aOwnerObjectMetaInfo aResult aValueType aNonVirtualValueType aDomain aType aHomeForIdCounter aBlock aNewObjectMetaInfo |

	theObject isNil ifTrue: [ ^nil].
	theBlock isNil ifTrue: [ ^nil].

	aBlock := [ | aRes |
		(CMTransaction  newTransactionDo: [   | |
 			aRes := theBlock value.
			self updateObject: theObject value: theNewObject.
		]) 
			ifTrue: [aRes]
			ifFalse: [ nil]
	].

	theNewObject isNil ifTrue: [  ^aBlock value].

	aType := self type.
	aType isNil ifTrue: [ ^nil].

	aType isDomainType ifTrue: [  ^theBlock value].

	aType isRequiredFeaturesType ifTrue: [ ^aBlock value].
	self isInitializationPropagationOnConnectAllowed ifFalse: [ ^aBlock value].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^nil].

	aValueType isDomainType ifTrue: [  ^theBlock value].

	aNonVirtualValueType := aValueType nonVirtualType.
	aNonVirtualValueType isNil ifTrue: [ ^nil].

	(aNonVirtualValueType isPrimitive or: [ aNonVirtualValueType isEnumeration]) ifTrue: [ 
  		^aBlock value
	].

	aNonVirtualValueType name asSymbol = CODEType name asSymbol ifTrue: [ ^theBlock value].

	aNewObjectMetaInfo :=  theNewObject metaInfo.
	aNewObjectMetaInfo isNil ifTrue: [ ^nil].	

	aBlock := [ | aRes |
		(CMTransaction  newTransactionDo: [   | |
 			aRes := theBlock value.
			aNewObjectMetaInfo initialize: theNewObject afterConnectionTo: theObject.
			self updateObject: theObject value: theNewObject.
		]) 
			ifTrue: [aRes]
			ifFalse: [ nil]
	].
	
	aOwnerObjectMetaInfo :=  theObject metaInfo.
	aOwnerObjectMetaInfo isNil ifTrue: [ ^nil].
				
	aDomain := aOwnerObjectMetaInfo searchDomainFrom: theObject.
	aDomain isNil ifTrue:[ ^aBlock value].

	aBlock := [ | aRes |
		(CMTransaction  newTransactionDo: [   | aAttributeObjectDomain |
 			aRes := theBlock value.
			aNewObjectMetaInfo initialize: theNewObject afterConnectionTo: theObject.
			self updateObject: theObject value: theNewObject.

			self name = self class homeRootsCMGODomainAttributeName 
				ifFalse: [ aNewObjectMetaInfo anchorNewObject: theNewObject inDomain: aDomain]
				ifTrue: [
					aAttributeObjectDomain := aNewObjectMetaInfo attributeOrInheritedNamed:  self class objectDomainCMGOAttributeName.
					aAttributeObjectDomain isNil ifFalse: [
						aAttributeObjectDomain object: theNewObject setTC: aDomain.
					]
				 ]
		]) 
			ifTrue: [aRes]
			ifFalse: [ nil]
	].

	aHomeForIdCounter := aNewObjectMetaInfo getHomeForIdCounterInDomain: aDomain.
	aHomeForIdCounter isNil ifTrue: [ 
		aHomeForIdCounter := aOwnerObjectMetaInfo searchHomeForIdCounterFrom: theObject inDomain: aDomain.
		aHomeForIdCounter isNil ifTrue: [ ^aBlock value]
	].
	
		
	aResult := nil.
	^(CMTransaction  newTransactionDo: [  | aAttributeObjectDomain  |

		aResult := theBlock value .
		aNewObjectMetaInfo initialize: theNewObject afterConnectionTo: theObject.
		self updateObject: theObject value: theNewObject.
		aNewObjectMetaInfo initializeNewObjectIDAttributes: theNewObject inHome: aHomeForIdCounter.

		self name = self class homeRootsCMGODomainAttributeName 
			ifFalse: [ aNewObjectMetaInfo anchorNewObject: theNewObject inDomain: aDomain]
			ifTrue: [
				aAttributeObjectDomain := aNewObjectMetaInfo attributeOrInheritedNamed:  self class objectDomainCMGOAttributeName.
				aAttributeObjectDomain isNil ifFalse: [
					aAttributeObjectDomain object: theNewObject setTC: aDomain.
				]
			 ]
	]) 
		ifTrue: [aResult]
		ifFalse: [ nil]! !

!CODEAttribute publicMethodsFor: 'TRF-object accessing'!

addToCollection: theCollection object: theObject 
	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	^theCollection add: theObject!

addToCollection: theCollection object: theObject before: theBeforeObject

	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	theBeforeObject isNil ifTrue: [ ^self addToCollection: theCollection object: theObject].

	^theCollection add: theObject before: theBeforeObject!

collection: theCollection includes: theObject

	theCollection isNil ifTrue: [ ^false].
	theObject isNil ifTrue: [ ^false].

	^theCollection includes: theObject!

getObjectAttributeValue: theObject
	| aValue aResult |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].

	self computationKind = self class computationKindAlways ifTrue: [ 
		aResult := self computeDerivedValueFrom: theObject expression: self initializationExpression.
		^aResult isNil 
			ifTrue: [ nil]
			ifFalse: [ 
				self isMultiplicityMany
					ifTrue: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult ]]
					ifFalse: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult asArray first]]
			]
	].

	aValue := theObject propertyAt: self name asSymbol.

	aValue = self class alreadyInitializedSentinelValue ifTrue: [
		^self isMultiplicityMany 
			ifFalse:[ nil]
			ifTrue: [ self isOrdered ifTrue: [ OrderedCollection new: 1] ifFalse: [ IdentitySet new: 3]]
	].
	
	aValue isNil ifFalse: [
		^self isMultiplicityMany 
			ifFalse:[ aValue]
			ifTrue: [ aValue copy]
	].

	^self isMultiplicityMany 
		ifFalse:[ nil]
		ifTrue: [ self isOrdered ifTrue: [ OrderedCollection new: 1] ifFalse: [ IdentitySet new: 3]]!

getObjectAttributeValueTC: theObject

	(self objectInstanceHasFeature: theObject)  ifFalse: [ ^self isMultiplicityMany ifTrue: [ Array new] ifFalse: [ nil]].

	^self getObjectAttributeValue: theObject!

getRelatedObjectNameValueNoDefault: theObject

	| aRelatedObject anObjectMetaInfo |
	theObject isNil ifTrue: [ ^nil].

	aRelatedObject := self getObjectFeatureValueTC: theObject.
	aRelatedObject isNil ifTrue: [ ^nil].

	anObjectMetaInfo := aRelatedObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [ ^nil].
	
	^anObjectMetaInfo getObjectNameValueNoDefault: aRelatedObject!

getTerminalObjectNameValue: theObject
	| aValueType |
self halt.
	theObject isNil ifTrue: [ ^nil].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^theObject printString].

	^aValueType getTerminalObjectNameValue: theObject!

hasBeenAlreadyTriedToInitialize: theObject
	| aValue |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^false].

	self computationKind = self class computationKindAlways ifTrue: [ ^false].

	aValue := theObject propertyAt: self name asSymbol.

	aValue = self class alreadyInitializedSentinelValue ifTrue: [ ^true].
	
	^aValue isNil not!

removeFromCollection: theCollection object: theObject 
	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	^theCollection remove: theObject ifAbsent: [ nil]!

restoreObject: theObject attributeValue: theRestoredValue
	theObject isNil ifTrue: [ ^nil].

	^(theObject isKindOf: CMGenericObject) 
		ifTrue: [ theObject propertyAt: self name asSymbol put: theRestoredValue]
		ifFalse: [ theObject perform: self restoreSelector with: theRestoredValue]!

setObject: theObject attributeValue: theNewObject
	theObject isNil ifTrue: [ ^nil].

	^(theObject isKindOf: CMGenericObject) 
		ifTrue: [ theObject propertyAt: self name asSymbol put: theNewObject]
		ifFalse: [ theObject perform: self setSelector with: theNewObject]! !

!CODEAttribute publicMethodsFor: 'TRF-object accessing-private'!

getNoInitObjectAttributeValue: theObject
	| aValue |
	theObject isNil ifTrue: [ ^nil].

	aValue := (theObject isKindOf: CMGenericObject) 
		ifTrue: [  theObject propertyAt: self name asSymbol]
		ifFalse: [ theObject perform: self getSelector].

	^aValue! !

!CODEAttribute publicMethodsFor: 'TRF-operations'!

getObjectFeatureValue: theObject

	^self getObjectAttributeValue: theObject!

getObjectFeatureValueTC: theObject

	^self getObjectAttributeValueTC: theObject!

object: theObject add: theNewObject 

	self isMultiplicityMany ifFalse: [ ^nil].
	
	^self initialize: theNewObject afterConnectionTo: theObject linkBlock: [
		CMGenericLinkMaker from: theObject to: theNewObject metaInfo: self
	]!

object: theObject add: theNewObject before: theOneBefore

	^self object: theObject before: theOneBefore add: theNewObject before: nil!

object: theObject addTC: theNewObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject)  ifFalse: [ ^nil].

	^self object: theObject add: theNewObject!

object: theObject addTC: theNewObject before: theOneBefore

	^self object: theObject before: theOneBefore addTC: theNewObject before: nil!

object: theObject before: theOneBefore add: theNewObject before: theOtherBefore

	self isMultiplicityMany ifFalse: [ ^nil].

	^self initialize: theNewObject afterConnectionTo: theObject linkBlock: [
		CMGenericLinkMaker from: theObject before: theOneBefore to: theNewObject before: theOtherBefore metaInfo: self
	]!

object: theObject before: theOneBefore addTC: theNewObject before: theOtherBefore

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject)  ifFalse: [ ^nil].

	^self object: theObject before: theOneBefore add: theNewObject before: theOtherBefore!

object: theObject remove: theExistingObject 

	self isMultiplicityMany ifFalse: [ ^nil].

	^CMGenericLinkMaker unlinkerFrom: theObject to: theExistingObject metaInfo: self!

object: theObject removeTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject)  ifFalse: [ ^nil].

	^self object: theObject remove: theExistingObject!

object: theObject set: theNewObject 

	self isMultiplicityOne ifFalse: [ ^nil].
	
	^self initialize: theNewObject afterConnectionTo: theObject linkBlock: [
		CMGenericLinkMaker from: theObject to: theNewObject metaInfo: self
	]!

object: theObject setTC: theNewObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject)  ifFalse: [ ^nil].

	^self object: theObject set: theNewObject!

object: theObject unset: theExistingObject 

	self isMultiplicityOne ifFalse: [ ^nil].

	^CMGenericLinkMaker unlinkerFrom: theObject to: theExistingObject metaInfo: self!

object: theObject unsetTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject)  ifFalse: [ ^nil].

	^self object: theObject unset: theExistingObject!

requiresIdCounter: theObject

	| aNonVirtualType aValueType anAttributeValue |

	theObject isNil ifTrue: [ ^false].

	self isExclussion ifTrue: [ ^false].
	self maxMult = self class maxMultNone ifTrue: [ ^false].

	self computationKind = self class computationKindInitializedFromHomeIDCounter ifFalse: [ ^false].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^false].

	aNonVirtualType := aValueType nonVirtualType.
	aNonVirtualType isNil  ifTrue: [ ^false].

	self isIDAttribute ifTrue: [ aNonVirtualType isIDCompatible ].

	aNonVirtualType isPrimitive ifTrue: [ ^false].

	anAttributeValue := self getObjectFeatureValueTC: theObject.
	anAttributeValue isNil ifTrue: [ ^false].

	^aValueType requiresIdCounter: anAttributeValue!

setObject: theObject featureValue: theNewObject
	^self setObject: theObject attributeValue: theNewObject!

setObject: theObject featureValue: theNewObject oldValue: theValue
self halt: 'This should work with Terminals multiplicity many'.
	^self setObject: theObject featureValue: theNewObject!

setObject: theObject featureValue: theNewObject withController: theController
	^self setObject: theObject featureValue: theNewObject! !

!CODEAttribute publicMethodsFor: 'TRF-parms'!

getSelector
	^(self reengineredClassName, 'Exact') asSymbol!

restoreSelector
	^(self reengineredClassName, 'Forced:') asSymbol!

setSelector
	^(self reengineredClassName, 'Forced:') asSymbol! !

!CODEAttribute publicMethodsFor: 'TRF-subActions'!

subActionPreviousToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doUnlink ifTrue: [ ^true].

	^true!

subActionSaveToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saSaveToDoLink: theLinker!

subActionTestToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doUnlink ifTrue: [ ^self subActionTestToDoUnLink: theLinker].

	theLinker doOrdered ifFalse: [ ^true].

	^self saTestToDoLink: theLinker!

subActionTestToDoUnLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saTestToDoUnLink: theLinker!

subActionToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doUnlink ifTrue: [ ^self subActionToDoUnLink: theLinker].

	^self saToDoLink: theLinker!

subActionToDoUnLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saToDoUnLink: theLinker!

subActionToUndoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saToUndoLink: theLinker!

subActionUniqTestToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doUnlink ifTrue: [ ^true].

	^self saUniqTestToDoLink: theLinker! !

!CODEAttribute publicMethodsFor: 'TRF-subActions-private'!

saSaveToDoLink: theLinker 

	| aCurrentlyLinked anObject aValueToSave |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone ifTrue: [ ^true].

	anObject := theLinker oneObject.
	anObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	aCurrentlyLinked := self getNoInitObjectAttributeValue: anObject.
	aValueToSave := aCurrentlyLinked isNil 
		ifTrue: [ nil] 
		ifFalse: [ 
			self isMultiplicityMany 
				ifTrue: [ aCurrentlyLinked copy] 
				ifFalse: [ aCurrentlyLinked]
		].
		
	theLinker save: aValueToSave side: #one.

	^true!

saTestToDoLink: theLinker 

	| aCurrentlyLinked aOneObject aOneBeforeObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].
	self isMultiplicityMany 	ifFalse: [ ^true].

	aOneObject := theLinker oneObject.
	aOneObject isNil ifTrue: [ ^self error: 'Can not unlink without object'].

	aCurrentlyLinked := self getNoInitObjectAttributeValue: aOneObject.
	(aCurrentlyLinked isNil or: [ aCurrentlyLinked isEmpty])  ifTrue: [ ^false].

	aOneBeforeObject := theLinker oneBefore.
	aOneBeforeObject isNil ifTrue: [ ^true].

	^self collection: aCurrentlyLinked includes: aOneBeforeObject!

saTestToDoUnLink: theLinker 

	| aCurrentlyLinked aOneObject anOtherObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone ifTrue: [ ^true].

	aOneObject := theLinker oneObject.
	aOneObject isNil ifTrue: [ ^self error: 'Can not unlink without object'].

	aCurrentlyLinked := self getNoInitObjectAttributeValue: aOneObject.
	(aCurrentlyLinked isNil or: [ 
		((aCurrentlyLinked isKindOf: Collection) and: [ 
			(aCurrentlyLinked isKindOf: String) not and: [  (aCurrentlyLinked isKindOf: CMGenericObject) not]]) and: [ aCurrentlyLinked isEmpty]])  ifTrue: [ ^false].

	anOtherObject := theLinker otherObject.
	anOtherObject isNil ifTrue: [ ^self error: 'Can not unlink without object'].

	^self isMultiplicityMany	
		ifTrue: [ self collection: aCurrentlyLinked includes: anOtherObject]
		ifFalse: [ aCurrentlyLinked == anOtherObject]!

saToDoLink: theLinker 

	| aCurrentlyLinked anOtherObject aOneObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	aOneObject := theLinker oneObject.
	aOneObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	anOtherObject := theLinker otherObject.
	anOtherObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	aCurrentlyLinked := self getNoInitObjectAttributeValue: aOneObject.

	(aCurrentlyLinked isNil and: [ self isMultiplicityMany]) ifTrue: [ 
		aCurrentlyLinked := self isOrdered ifTrue: [ OrderedCollection new: 1] ifFalse: [ IdentitySet new: 3].
		aOneObject propertyAt: self name asSymbol put: aCurrentlyLinked.
	].

	^self isMultiplicityMany	
		ifTrue: [ 
			theLinker doOrdered
				ifTrue: [  self addToCollection: aCurrentlyLinked object: anOtherObject before: theLinker beforeObject]
				ifFalse: [  self addToCollection: aCurrentlyLinked object: anOtherObject ].
			true
		]
		ifFalse: [  
			self setObject: aOneObject attributeValue: anOtherObject.
			true
		]!

saToDoUnLink: theLinker 

	| aCurrentlyLinked anOtherObject aOneObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	aOneObject := theLinker oneObject.
	aOneObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	anOtherObject := theLinker otherObject.
	anOtherObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	aCurrentlyLinked := self getNoInitObjectAttributeValue: aOneObject.
	(aCurrentlyLinked isNil or: [ 
		((aCurrentlyLinked isKindOf: Collection) and: [ 
			(aCurrentlyLinked isKindOf: String) not and: [  (aCurrentlyLinked isKindOf: CMGenericObject) not]]) and: [ aCurrentlyLinked isEmpty]])  ifTrue: [ ^false].

	^self isMultiplicityMany	
		ifTrue: [ 
			(self collection: aCurrentlyLinked includes: anOtherObject)
				ifFalse: [ false]
				ifTrue: [ 
					self removeFromCollection: aCurrentlyLinked object: anOtherObject.
					true
				]
		]
		ifFalse: [ 
			aCurrentlyLinked == anOtherObject 
				ifFalse: [ false]
				ifTrue: [ 
					self setObject: aOneObject attributeValue: nil.
					true
				]
		]!

saToUndoLink: theLinker 

	| anObject aSaved |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	anObject := theLinker oneObject.
	anObject isNil ifTrue: [ ^self error: 'Can not undo without object'].

	aSaved :=  theLinker oneSaved.

	self restoreObject: anObject attributeValue: aSaved.
	
	^true!

saToUndoLink: theLinker side: theSide

	| anObject aSaved |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	anObject := theLinker oneObject.
	anObject isNil ifTrue: [ ^self error: 'Can not undo without object'].

	aSaved :=  theLinker oneSaved.

	self restoreObject: anObject attributeValue: aSaved.
	
	^true!

saUniqTestToDoLink: theLinker 

	| aCurrentlyLinked aOneObject anOtherObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone ifTrue: [ ^true].
	self isMultiplicityOne ifTrue: [ ^true ].
	self isDuplicatesAllowed ifTrue: [ ^true].

	aOneObject := theLinker oneObject.
	aOneObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	aCurrentlyLinked := self getNoInitObjectAttributeValue: aOneObject.
	(aCurrentlyLinked isNil or: [ aCurrentlyLinked isEmpty])  ifTrue: [ ^true].

	anOtherObject :=  theLinker otherObject.
	anOtherObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	^(aCurrentlyLinked includes:  anOtherObject) not! !

!CODEAttribute publicMethodsFor: 'TRF-type membership'!

objectInstanceHasFeature: theObject 

	| aMetaInfo aResult |

	theObject isNil ifTrue: [ ^false].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^false].

	aResult := aMetaInfo allEffectiveAttributes includes: self.
	^aResult!

objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject 

	^(self objectInstanceHasFeature: theObject) and: [ self relatedIsTypeOfObjectInstance: theNewObject]!

relatedIsTypeOfObjectInstance: theNewObject 

	| aValueType aResult |

	theNewObject isNil ifTrue: [ ^false].

	aValueType := self valueType.
	aValueType isNil ifTrue: [ ^false].
	
	aResult := aValueType isTypeOfObjectInstance: theNewObject.
	^aResult! !

!CODEElement class publicMethodsFor: 'CMGO constants'!

argumentSignatureAttributeNamePostfix
	^'_ArgumentCMGO' copy!

candidatesFeatureNameForRequiredReferencedFeaturePostfix
	^ '_CandidatesRequiredReferencedFeatureCMGO' copy!

CMGOTypeModuleNames
	^Array with: 'PrimitiveTypes' copy!

CMGOTypeName
	^'CMGO' copy!

domainCMGOHomeRelationshipName
	^'domainCMGO' copy!

domainCMGOModuleNames
	^Array with: 'DomainRootElements' copy!

domainCMGOTypeName
	^'Domain' copy!

domainModelCMGODomainAttributeName
	^'domainModelCMGO' copy!

domainNameCMGODomainAttributeName
	^'domainNameCMGO' copy!

homeCMGOModuleNames
	^Array with: 'DomainRootElements' copy!

homeCMGOTypeName
	^'Home' copy!

homedElementsTypeMetaInfoCMGODomainAttributeName
	^'homedElementsTypeMetaInfoCMGO' copy!

homeIDCounterCMGODomainAttributeName
	^'homeIDCounterCMGO' copy!

homeNameCMGODomainAttributeName
	^'homeNameCMGO' copy!

homeRootsCMGODomainAttributeName
	^'homeRootsCMGO' copy!

homesCMGODomainRelationshipName
	^'homesCMGO' copy!

objectDomainCMGOAttributeName
	^'objectDomainCMGO' copy!

operationSignatureAttributeNamePostfix
	^'_OperationCMGO' copy!

requiredFeaturesParentAttributeName
	^'requiredFeaturesParentAttributeCMGO' copy!

requiredFeaturesTypePostfix
	^'_RequiredFeaturesCMGO' copy!

requiredFeaturesTypesModuleNames
	^Array with: 'Aux' copy with: 'RequiredFeaturesTypes_Module' copy!

specializedHomeTypesModuleNames
	^Array with: 'Aux' copy with: 'HomeTypes_Module' copy!

typeSignatureAttributeNamePostfix
	^'_SignatureCMGO' copy! !

!CODEElement class publicMethodsFor: 'expression parsing'!

calcSubExpressions: theStrings
	^self calcSubExpressionsParseNested: theStrings!

calcSubExpressionsNoParseNested: theStrings
	| someSubExpressions aCurrentSubExpression |

	theStrings isNil ifTrue: [ ^nil].
	(theStrings isNil or: [ theStrings  size < 2]) ifTrue: [ ^nil].

	someSubExpressions := OrderedCollection new: theStrings size.
	aCurrentSubExpression := OrderedCollection new: theStrings size.

	theStrings do: [:aString |
		(self isExpressionOperator:  aString) 
			ifTrue: [ 
				aCurrentSubExpression isEmpty ifFalse: [ 
					someSubExpressions add: aCurrentSubExpression; add: aString.
					aCurrentSubExpression := OrderedCollection new: theStrings size.
				]
			]
			ifFalse: [ aCurrentSubExpression add: aString	]
	].
	aCurrentSubExpression isEmpty ifFalse: [ 
		someSubExpressions add:  aCurrentSubExpression
	].

	someSubExpressions  size < 2 ifTrue: [ ^nil].
 
	^someSubExpressions!

calcSubExpressionsParseNested: theStrings
	"self calcSubExpressionsParseNested: 'a + ( b + c ) + e' asArrayOfSubstrings"

	| someSubExpressions aCurrentSubExpression aExpressionsStack  |

	(theStrings isNil or: [ theStrings  isEmpty]) ifTrue: [ ^nil].

	aExpressionsStack := OrderedCollection new: 4.
	someSubExpressions := OrderedCollection new: 16.
	aExpressionsStack addLast: someSubExpressions.
	aCurrentSubExpression := OrderedCollection new: 16.

	theStrings do: [:aString | | aSymbol someNestedSubExpressions | 
		(self isExpressionOperator:  aString) 
			ifTrue: [ 
				aSymbol := aString asSymbol.
				aSymbol = self initExpressionStartSubExpression
					ifTrue: [ 
						aCurrentSubExpression isEmpty ifFalse: [ 
							someSubExpressions add: aCurrentSubExpression
						].
						someNestedSubExpressions := OrderedCollection new: 16.
						someSubExpressions add: someNestedSubExpressions.
						someSubExpressions := someNestedSubExpressions.
						aExpressionsStack addLast: someSubExpressions.
						someSubExpressions add: aString.				
						aCurrentSubExpression := OrderedCollection new: 16.
					]
					ifFalse: [ 
						aSymbol = self initExpressionEndSubExpression
							ifTrue: [ 
								aCurrentSubExpression isEmpty ifFalse: [ 
									someSubExpressions add: aCurrentSubExpression
								].
								someSubExpressions add: aString.				
								aExpressionsStack size < 2
									ifTrue:  [ " is an error, but go on..."
										someSubExpressions := OrderedCollection new: 16
									]
									ifFalse: [  
										aExpressionsStack removeLast.
										someSubExpressions := aExpressionsStack last.
									].
								aCurrentSubExpression := OrderedCollection new: 16.
							]
							ifFalse: [ 
								aCurrentSubExpression isEmpty ifFalse: [ 
									someSubExpressions add: aCurrentSubExpression.
									aCurrentSubExpression := OrderedCollection new: 16.
								].
								someSubExpressions add: aString.

							]
					]
			]
			ifFalse: [ aCurrentSubExpression add: aString	]
	].
	aCurrentSubExpression isEmpty ifFalse: [ 
		someSubExpressions add:  aCurrentSubExpression
	].

	aExpressionsStack isEmpty ifTrue: [ ^nil].
	^aExpressionsStack first!

pathExpressionsFromCalcSubExpressions: theStrings
	^self pathExpressionsFromCalcSubExpressionsParseNested: theStrings!

pathExpressionsFromCalcSubExpressionsNoParseNested: theStrings
	| someSubExpressions somePaths |

	theStrings isNil ifTrue: [ ^nil].
	(theStrings isNil or: [ theStrings  size < 2]) ifTrue: [ ^nil].
self halt. 
	someSubExpressions := self calcSubExpressionsNoParseNested: theStrings.
	someSubExpressions  size < 2 ifTrue: [ ^nil].

	somePaths := OrderedCollection new: someSubExpressions size.
 
	someSubExpressions do: [:aSubExpression |  
		((aSubExpression isKindOf: String) and: [self isExpressionOperator: aSubExpression])  ifFalse: [ 
			(aSubExpression first first = $"  or: [  aSubExpression first first isDigit ]) ifFalse: [ 
				somePaths add: aSubExpression
			]
		]
	].

	^somePaths!

pathExpressionsFromCalcSubExpressionsParseNested: theStrings
	| somePathExpressions |

	theStrings isNil ifTrue: [ ^nil].

	somePathExpressions := OrderedCollection new: theStrings size.
 
	self pathExpressionsFromCalcSubExpressionsParseNested: theStrings into: somePathExpressions.
	^somePathExpressions!

pathExpressionsFromCalcSubExpressionsParseNested: 	theSubExpressions into: thePathExpressions

	theSubExpressions isNil  ifTrue: [ ^nil].

	theSubExpressions do: [:aSubExpression |   | someNestedSubExpressions  |
		((aSubExpression isKindOf: String) and: [self isExpressionOperator: aSubExpression]) 
			ifTrue: [ ]
			ifFalse: [ 		
				aSubExpression first = $" 
					ifTrue: [ ]
					ifFalse: [ 
						(aSubExpression first asSymbol = self initExpressionStartSubExpression and: [ aSubExpression last  asSymbol = self  initExpressionEndSubExpression])
							ifTrue: [ self halt.
								aSubExpression size < 3 
									ifTrue: [ nil]
									ifFalse: [ 
										someNestedSubExpressions := aSubExpression copyFrom: 2 to: aSubExpression size - 1.
										self pathExpressionsFromCalcSubExpressionsParseNested: someNestedSubExpressions into: thePathExpressions
									]
							]
							ifFalse: [ 
								aSubExpression first isDigit 
									ifTrue:  []
									ifFalse: [ 
										thePathExpressions add: (OrderedCollection new add: aSubExpression; yourself)
									]
							]
					].
				
			]
	].! !

!CODEElement class publicMethodsFor: 'preferences'!

preferredTransactionClass
	^self preferredPreferencesClass preferredTransactionClass! !

!CODEElement class publicMethodsFor: 'TRF-derivations'!

compute: theValue operator: theOperator

	| anOperator |
	(theOperator isNil or: [ theOperator isEmpty]) ifTrue: [ ^theValue].

	anOperator := theOperator asSymbol.
	
	(self isUnaryExpressionOperator: anOperator)  ifFalse: [ ^nil].

	Object errorSignal 
		handle: [:anEx | self halt: 'Error in Expression operator ', anOperator]
		do: [  | aValue |
			aValue := (theValue isNil not and: [ (theValue isKindOf: Collection) and: [ (theValue isKindOf: String) not]])
				ifTrue: [ theValue isEmpty ifTrue: [ nil] ifFalse: [ theValue asArray first]]
				ifFalse: [ theValue].

			anOperator = self initExpressionObjectOperatorIsNil ifTrue: [ ^aValue isNil].
			anOperator = self  initExpressionObjectOperatorNotNil ifTrue: [ ^aValue isNil not].
			anOperator = self  initExpressionBooleanOperatorNot ifTrue: [ ^(aValue == true) not].
			anOperator = self  initExpressionSetOperatorSize ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue size] ifFalse: [ 0]].
			anOperator = self  initExpressionSetOperatorFirst ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue isEmpty ifTrue:  [ nil] ifFalse: [ theValue asOrderedCollection first]] ifFalse: [ nil]].
			anOperator = self  initExpressionSetOperatorLast ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue isEmpty ifTrue:  [ nil] ifFalse: [ theValue asOrderedCollection last]] ifFalse: [ nil]].
			anOperator = self  initExpressionStringOperatorIsEmpty ifTrue: [ 
				^(aValue isKindOf: Collection) ifTrue: [ theValue isEmpty] ifFalse: [ theValue isNil]].
			anOperator = self  initExpressionStringOperatorNotEmpty ifTrue: [ 
				^(aValue isKindOf: Collection) ifTrue: [ theValue isEmpty not] ifFalse: [ theValue isNil not]].

			anOperator = self  initExpressionSetOperatorIsEmpty ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue isEmpty] ifFalse: [ theValue isNil]].
			anOperator = self  initExpressionSetOperatorNotEmpty ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue isEmpty not] ifFalse: [ theValue isNil not]].
			anOperator = self  initExpressionStringOperatorSize ifTrue: [ 
				^(aValue isKindOf: Collection) ifTrue: [ theValue size] ifFalse: [ 0]].
			anOperator = self  initExpressionSetOperationSum ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue inject: 0 into: [:acum :elem | acum + elem]] ifFalse: [ 0]].
			anOperator = self  initExpressionSetOperatorMax ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue inject: 0 into: [:max :elem | elem > max ifTrue: [ elem] ifFalse: [ max] ]] ifFalse: [ 0]].
			anOperator = self  initExpressionSetOperatorMin ifTrue: [ 
				^(theValue isKindOf: Collection) ifTrue: [ theValue inject: Number maxInteger into: [:min :elem | elem < min ifTrue: [ elem] ifFalse: [ min] ]] ifFalse: [ 0]].

			anOperator = self  initExpressionCastOperatorFloor ifTrue: [ 
				^aValue floor].


		].
	^nil!

compute: theValue operator: theOperator value: theArgumentValue

	| aSet anOperator |
	(theOperator isNil or: [ theOperator isEmpty]) ifTrue: [ ^theValue].

	anOperator := theOperator asSymbol.

	(self isBinaryExpressionOperator: anOperator)  ifFalse: [ ^nil].
	
	Object errorSignal 
		handle: [:anEx | self halt: 'Error in Expression operator ', anOperator]
		do: [  | aValue anArgumentValue |
			aValue := (theValue isNil not and: [ (theValue isKindOf: Collection) and: [ (theValue isKindOf: String) not]])
				ifTrue: [ theValue isEmpty ifTrue:[ nil] ifFalse: [ theValue asArray first]]
				ifFalse: [ theValue].
			aValue := (aValue isKindOf: CMGenericObject)
				ifFalse: [ aValue]
				ifTrue: [ 
					aValue enumValue isNil 
						ifFalse: [ aValue enumValue name]
						ifTrue: [ aValue metaInfo getObjectNameValue: aValue]
				].
			anArgumentValue := (theArgumentValue isNil not and: [ 
				(theArgumentValue isKindOf: Collection) and: [ (theArgumentValue isKindOf: String) not]])
				ifTrue: [ theArgumentValue isEmpty ifTrue:[ nil] ifFalse: [ theArgumentValue asArray first]]
				ifFalse: [ theArgumentValue].
			anArgumentValue := (anArgumentValue isKindOf: CMGenericObject)
				ifFalse: [ anArgumentValue]
				ifTrue: [ 
					anArgumentValue enumValue isNil 
						ifFalse: [ anArgumentValue enumValue name]
						ifTrue: [ anArgumentValue metaInfo getObjectNameValue: anArgumentValue]
				].
			anOperator = self initExpressionBooleanOperatorSame ifTrue: [  
				^(aValue isNil or: [ anArgumentValue isNil]) 
					ifTrue: [ aValue == anArgumentValue]
					ifFalse: [ 
					((aValue isKindOf: CMGenericObject) and: [ aValue enumValue isNil not]) 
					ifTrue: [
						((anArgumentValue isKindOf: CMGenericObject) and: [ anArgumentValue enumValue isNil not]) 
							ifTrue: [ aValue enumValue == aValue enumValue ]
							ifFalse: [ aValue enumValue name = anArgumentValue ]
					]
					ifFalse: [
						((anArgumentValue isKindOf: CMGenericObject) and: [ anArgumentValue enumValue isNil not]) 
							ifTrue: [ anArgumentValue enumValue name = aValue ]
							ifFalse: [ aValue = anArgumentValue]
					]
					]
			].
			anOperator = self initExpressionBooleanOperatorAnd ifTrue: [  
				^aValue == true and: [ anArgumentValue == true]].
			anOperator = self initExpressionBooleanOperatorOr ifTrue: [  
				^aValue == true or: [ anArgumentValue == true]].
			anOperator = self initExpressionBooleanOperatorXor ifTrue: [  
				^aValue == true xor: [ anArgumentValue == true]].
			anOperator = self initExpressionArithmeticOperatorAdd ifTrue: [  
				^((aValue isKindOf: Number) ifTrue: [ aValue] ifFalse: [ 0]) + 
					((anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue] ifFalse: [ 0])].
			anOperator = self initExpressionArithmeticOperatorSubstract ifTrue: [  
				^((aValue isKindOf: Number) ifTrue: [ aValue] ifFalse: [ 0]) - 
					((anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue] ifFalse: [ 0])].
			anOperator = self initExpressionArithmeticOperatorMult ifTrue: [  
				^((aValue isKindOf: Number) ifTrue: [ aValue] ifFalse: [ 0]) *  
					((anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue] ifFalse: [ 0])].
			anOperator = self initExpressionArithmeticOperatorDiv ifTrue: [ 
				^((aValue isKindOf: Number) ifTrue: [ aValue] ifFalse: [ 0]) / 
					((anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue] ifFalse: [ 1])].
			anOperator = self initExpressionArithmeticOperatorPercentage ifTrue: [  
				^((aValue isKindOf: Number) ifTrue: [ aValue] ifFalse: [ 0]) *  
					((anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue] ifFalse: [ 0]) / 100].
			anOperator = self initExpressionArithmeticOperatorMin ifTrue: [  
				^((aValue isKindOf: Number) ifTrue: [ aValue] ifFalse: [ 0]) min:  
					((anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue] ifFalse: [ 0])].
			anOperator = self initExpressionArithmeticOperatorMax ifTrue: [  
				^((aValue isKindOf: Number) ifTrue: [ aValue] ifFalse: [ 0]) max:  
					((anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue] ifFalse: [ 0])].
			anOperator = self initExpressionComparisonOperatorLT ifTrue: [ 
				^aValue < anArgumentValue
			].
			anOperator = self initExpressionComparisonOperatorLTE ifTrue: [ 
				^aValue <= anArgumentValue
			].
			anOperator = self initExpressionComparisonOperatorEqual ifTrue: [ 
				^aValue = anArgumentValue
			].
			anOperator = self initExpressionComparisonOperatorIdentical ifTrue: [ 
				^aValue == anArgumentValue
			].
			anOperator = self initExpressionComparisonOperatorGT ifTrue: [ 
				^aValue > anArgumentValue
			].
			anOperator = self initExpressionComparisonOperatorGTE ifTrue: [ 
				^aValue >= anArgumentValue
			].

			anOperator = self initExpressionStringOperatorConcat ifTrue: [  
				^((aValue isKindOf: String) ifTrue: [ aValue] ifFalse: [ String new]) asString,  
					((anArgumentValue isKindOf: String) ifTrue: [ anArgumentValue] ifFalse: [(anArgumentValue isKindOf: Number) ifTrue: [ anArgumentValue printString] ifFalse: [ String new]]) asString].
			anOperator = self initExpressionStringOperatorStartsWith ifTrue: [  | aTmp otherTmp |
				aTmp := ((aValue isKindOf: String) ifTrue: [ aValue] ifFalse: [ String new]) asString.
				otherTmp := ((anArgumentValue isKindOf: String) ifTrue: [ anArgumentValue] ifFalse: [ String new]) asString.
				^aTmp size >= otherTmp size and: [ 
					((1 to: otherTmp size) detect: [:anIndex | ((aTmp at: anIndex) = (otherTmp at: anIndex)) not] ifNone: [ nil]) isNil]].
			anOperator = self initExpressionStringOperatorEndsWith ifTrue: [  | aTmp otherTmp |
				aTmp := ((aValue isKindOf: String) ifTrue: [ aValue] ifFalse: [ String new]) asString.
				otherTmp := ((anArgumentValue isKindOf: String) ifTrue: [ anArgumentValue] ifFalse: [ String new]) asString.
				^aTmp size >= otherTmp size and: [ 
					((aTmp size - otherTmp size + 1 to: aTmp size) detect: [:anIndex | ((aTmp at: anIndex) = (otherTmp at: anIndex)) not] ifNone: [ nil]) isNil]].
			anOperator = self initExpressionStringOperatorMatch ifTrue: [  | aTmp otherTmp |
				aTmp := ((aValue isKindOf: String) ifTrue: [ aValue] ifFalse: [ String new]) asString.
				otherTmp := ((anArgumentValue isKindOf: String) ifTrue: [ anArgumentValue] ifFalse: [ String new]) asString.
				^aTmp match: otherTmp].


			anOperator = self initExpressionSetOperatorUnion ifTrue: [ 
				aSet := IdentitySet new: 13.
				aSet addAll: ((theValue isKindOf: Collection) ifTrue: [ theValue] ifFalse: [ theValue isNil ifTrue: [Set new] ifFalse: [ Set new add: theValue; yourself]]).
				aSet addAll: ((theArgumentValue isKindOf: Collection) ifTrue: [ theArgumentValue] ifFalse: [ theArgumentValue isNil ifTrue: [Set new] ifFalse: [ Set new add: theValue; yourself]]).
				^aSet].
			anOperator = self initExpressionSetOperatorIntersection ifTrue: [  | aTmp |
				aTmp := ((theArgumentValue isKindOf: Collection) ifTrue: [ theArgumentValue] ifFalse: [ Set new]) asSet.
				^((theValue isKindOf: Collection) ifTrue: [ theValue] ifFalse: [ Set new]) asSet select: [:anElem | aTmp includes: anElem] ].
			anOperator = self initExpressionSetOperatorDifference ifTrue: [ 
				aSet := IdentitySet new: 13.
				aSet addAll: ((theValue isKindOf: Collection) ifTrue: [ theValue] ifFalse: [ Set new]).
				aSet removeAll: ((theArgumentValue isKindOf: Collection) ifTrue: [ theArgumentValue] ifFalse: [ Set new]).
				^aSet].

			anOperator = self initExpressionSetOperatorConcat ifTrue: [ 
				aSet := OrderedCollection new: 10.
				aSet addAll: ((theValue isKindOf: Collection) ifTrue: [ theValue] ifFalse: [ Array new]).
				aSet addAll: ((theArgumentValue isKindOf: Collection) ifTrue: [ theArgumentValue] ifFalse: [ Array new]).
				^aSet ].

			anOperator = self initExpressionSetOperatorIncludes ifTrue: [  
				anArgumentValue isNil ifTrue: [ ^false].
				^((theValue isKindOf: Collection) ifTrue: [ aValue] ifFalse: [ Set new]) includes: anArgumentValue].

		].
	^nil!

recurse: theObjectCollection expression: theExpression

	| someCollectedObjects aValue aRecursedValue anExpression |

	(theObjectCollection isNil or: [ theObjectCollection isEmpty]) ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	anExpression := Array with: theExpression last.
	someCollectedObjects := OrderedCollection new: theObjectCollection size.

	theObjectCollection do: [:anObject |
		(anObject isNil not and: [ (anObject isKindOf: CMGenericObject) and: [ anObject metaInfo isEnumeration not]]) 
			ifFalse: [  anObject  isNil ifFalse: [  someCollectedObjects add: anObject]]
			ifTrue: [ 
				aValue := anObject metaInfo object: anObject derive: anExpression clone: false.
				(aValue isNil not and: [ aValue isEmpty not]) ifTrue: [ 
					aRecursedValue := self recurseCollect: aValue expression: anExpression.
					(aRecursedValue isNil not and: [ aRecursedValue isEmpty not]) 
						ifTrue: [  someCollectedObjects addAll: aRecursedValue]
						ifFalse: [  someCollectedObjects addAll: aValue]
				]
			]
	].
	someCollectedObjects isEmpty ifTrue: [ ^theObjectCollection].

	^someCollectedObjects!

recurseCollect: theObjectCollection expression: theExpression

	| someCollectedObjects aValue aRecursedValue anExpression |

	(theObjectCollection isNil or: [ theObjectCollection isEmpty]) ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	anExpression := Array with: theExpression last.
	someCollectedObjects := OrderedCollection new: theObjectCollection size * 2.
	someCollectedObjects addAll: theObjectCollection.

	theObjectCollection do: [:anObject |
		(anObject isNil not and: [ (anObject isKindOf: CMGenericObject) and: [ anObject metaInfo isEnumeration not]]) 
			ifFalse: [  anObject  isNil ifFalse: [  someCollectedObjects add: anObject]]
			ifTrue: [ 
				aValue := anObject metaInfo object: anObject derive: anExpression clone: false.
				(aValue isNil not and: [ aValue isEmpty not]) ifTrue: [ 
					aRecursedValue := self recurse: aValue expression: anExpression.
					(aRecursedValue isNil not and: [ aRecursedValue isEmpty not]) 
						ifTrue: [  someCollectedObjects addAll: aRecursedValue]
						ifFalse: [  someCollectedObjects addAll: aValue]
				]
			]
	].
	someCollectedObjects isEmpty ifTrue: [ ^theObjectCollection].

	^someCollectedObjects! !

!CODEElement publicMethodsFor: 'preferences'!

preferredTransactionClass
	^self class preferredTransactionClass! !

!CODEModel publicMethodsFor: 'TRF-operations'!

createDomainObject

	| aTypeDomain aDomainCMGO someTypes aAttributeDomainName aAttributeDomainModel |
	aTypeDomain := self resolveReferencedTypeName: self class domainCMGOTypeName moduleNames: self class domainCMGOModuleNames.
	aTypeDomain isNil ifTrue: [ ^nil].

	aDomainCMGO := aTypeDomain createObject.

	aAttributeDomainName := aTypeDomain attributeOrInheritedNamed:  self class domainNameCMGODomainAttributeName.
	aAttributeDomainName isNil ifTrue: [ ^nil].

	aAttributeDomainName object: aDomainCMGO setTC: self name copy.

	aAttributeDomainModel := aTypeDomain attributeOrInheritedNamed:  self class domainModelCMGODomainAttributeName.
	aAttributeDomainModel isNil ifTrue: [ ^nil].

	aAttributeDomainModel object: aDomainCMGO setTC: self.


	someTypes := self allTypes.
	someTypes do: [:aType |
		aType createHomeObjectInDomain: aDomainCMGO
	].

	^aDomainCMGO!

createSpecializedHomeTypes

	| someTypes |
	someTypes := self allTypes.
	someTypes do: [:aType |
		aType createSpecializedHomeType
	].! !

!CODEOperation publicMethodsFor: 'TRF-feature accessing'!

executeFrom: theObject expression: theExpression
	| someStrings aDerivationPath aMetaInfo aType |

	theObject isNil ifTrue: [ ^nil].

	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.

	someStrings first =  self class initExpressionNilValue  ifTrue: [ 
		^nil
	].

	someStrings first =  self class initExpressionLiteralValue  ifTrue: [
		aType := self type.
		^aType isNil 
			ifTrue: [ nil]
			ifFalse: [ aType computeDerivedValueFromLiteralExpression: theExpression]
	].
 
	someStrings first =  self class initExpressionRandomAndLiteralValue  ifTrue: [
		aType := self type.
		^aType isNil 
			ifTrue: [ nil]
			ifFalse: [ aType computeDerivedValueFromRandomAndLiteralExpression: theExpression]
	].

	someStrings  size < 2 ifTrue: [ ^nil].

	aDerivationPath := someStrings copyFrom: 2 to: someStrings size.
	aDerivationPath isEmpty ifTrue: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	someStrings first asSymbol =  self class initExpressionCalc  ifTrue: [
		^aMetaInfo computeDerivedValueFrom: theObject calcExpression: theExpression
	].

	someStrings first =  self class initExpressionSmalltalk  ifTrue: [
		^aMetaInfo executeFrom: theObject smalltalkExpression: theExpression
	].

	^nil!

getObjectFeatureValue: theObject

	^self performObjectOperation: theObject!

getObjectFeatureValueTC: theObject

	^self performObjectOperationTC: theObject!

performObjectOperation: theObject
	| aReturn |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].
 
	aReturn := self type computeDerivedValueFrom: theObject expression: self executableExpression.
	
	aReturn isNil ifFalse: [ ^aReturn].

	^self isMultiplicityMany 
		ifFalse:[ nil]
		ifTrue: [ self isOrdered ifTrue: [ OrderedCollection new: 1] ifFalse: [ IdentitySet new: 3]]!

performObjectOperationTC: theObject

	theObject isNil ifTrue: [ ^self isMultiplicityMany ifTrue: [ Array new] ifFalse: [ nil]].

	(self objectInstanceHasFeature: theObject)  ifFalse: [ ^self isMultiplicityMany ifTrue: [ Array new] ifFalse: [ nil]].

	^self performObjectOperation: theObject!

performObjectOperationTC: theObject   argumentsHolder: theArgumentsHolder! !

!CODEOperation publicMethodsFor: 'TRF-type membership'!

objectInstanceHasFeature: theObject 

	| aMetaInfo aResult |

	theObject isNil ifTrue: [ ^false].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^false].

	aResult := aMetaInfo allEffectiveOperations includes: self.
	^aResult! !

!CODERelationship class publicMethodsFor: 'constants'!

movementBottomSymbol
	^#bottom!

movementDownSymbol
	^#down!

movementRelativeSymbol
	^#relative!

movementTopSymbol
	^#top!

movementUpSymbol
	^#up! !

!CODERelationship publicMethodsFor: 'TRF-change events'!

prChangeEventsFromLinker: theLinker side: theSide

	| anObject aSaved aMetaInfo |

	theLinker isNil ifTrue: [ ^self].

	self isMultiplicityNone 	ifTrue: [ ^self].

	anObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	anObject isNil ifTrue: [ ^self].

	aSaved := theSide = #one ifTrue: [ theLinker oneSaved] ifFalse: [ theLinker otherSaved].

	aMetaInfo := anObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo propagateChangeEventsFromObject: anObject feature: self saved: aSaved.

	self isSynthetizedRelationship ifTrue: [
		self prChangeEventsSynthetizedObject: anObject
	].
	self isSynthesisRelationship ifTrue: [
		self prChangeEventsSynthesisObject: anObject
	]!

prChangeEventsSynthesisObject: theObject

	| aMetaInfo someSynthetizedRelationships someInverseSynthetizedRelationships aSynthesisRelationship aDerivedRelationship someDerivedObjects |

	theObject isNil ifTrue: [ ^self].

	self isSynthesisRelationship ifFalse: [ ^self].

	self isMultiplicityNone 	ifTrue: [ ^self].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	someSynthetizedRelationships := aMetaInfo synthetizedRelationships.
	someSynthetizedRelationships isNil ifFalse: [ 
		someSynthetizedRelationships do: [:aSynthetizedRelationship |
			aMetaInfo propagateChangeEventsFromObject: theObject feature: aSynthetizedRelationship saved: nil.
		]
	].	

	someInverseSynthetizedRelationships := aMetaInfo inverseSynthetizedRelationships.
	someInverseSynthetizedRelationships isNil ifFalse: [ 
		someInverseSynthetizedRelationships do: [:aInverseSynthetizedRelationship |  | anInverse aRelatedValue |
			anInverse := aInverseSynthetizedRelationship inverse.
			(anInverse isNil not and: [ anInverse isPropagateSynthesisRelationship]) ifTrue:[
				aRelatedValue := aInverseSynthetizedRelationship getObjectRelationshipValueTC: theObject.
				aRelatedValue isNil ifFalse: [ 
					aInverseSynthetizedRelationship isMultiplicityMany
						ifTrue: [   
							aRelatedValue do: [:aRelatedObject | 
								aRelatedObject metaInfo propagateChangeEventsFromObject: aRelatedObject feature: anInverse saved: nil.

							]
						]
						ifFalse: [ 
							aRelatedValue metaInfo propagateChangeEventsFromObject: aRelatedValue feature: anInverse saved: nil.
						]
				]
			]
		]
	].




	aSynthesisRelationship := aMetaInfo synthesisRelationship.
	aSynthesisRelationship isNil ifTrue: [ ^nil].

	aDerivedRelationship := aSynthesisRelationship inverse.
	aDerivedRelationship isNil ifTrue: [ ^nil].
	
	someDerivedObjects := aDerivedRelationship getObjectRelationshipValueTC: theObject.
	someDerivedObjects isNil ifFalse: [ 
		someDerivedObjects do: [:aDerivedObject |   | aDerivedObjectRelationship |
			aDerivedObjectRelationship := self sameOrEquivalentRelationshipOn: aDerivedObject.
			aDerivedObjectRelationship isNil ifFalse: [ 
				aDerivedObjectRelationship prChangeEventsSynthesisObject: aDerivedObject.
			]
		]
	]!

prChangeEventsSynthetizedObject: theObject

	| aMetaInfo aSynthesisRelationship aDerivedRelationship someDerivedObjects |

	theObject isNil ifTrue: [ ^self].

	self isSynthetizedRelationship ifFalse: [ ^self].

	self isMultiplicityNone 	ifTrue: [ ^self].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].


	aSynthesisRelationship := aMetaInfo synthesisRelationship.
	aSynthesisRelationship isNil ifTrue: [ ^nil].

	aDerivedRelationship := aSynthesisRelationship inverse.
	aDerivedRelationship isNil ifTrue: [ ^nil].
	
	someDerivedObjects := aDerivedRelationship getObjectRelationshipValueTC: theObject.
	someDerivedObjects isNil ifFalse: [ 
		someDerivedObjects do: [:aDerivedObject |   | aDerivedObjectMetaInfo aDerivedObjectRelationship |
			aDerivedObjectMetaInfo := aDerivedObject metaInfo.
			aDerivedObjectRelationship := self sameOrEquivalentRelationshipOn: aDerivedObject.
			aDerivedObjectRelationship isNil ifFalse: [ 
				aDerivedObjectMetaInfo propagateChangeEventsFromObject: aDerivedObject feature: aDerivedObjectRelationship saved: nil.

				aDerivedObjectRelationship prChangeEventsSynthetizedObject: aDerivedObject
			]
		]
	]!

propagateChangeEventsFromLinker: theLinker

	theLinker isNil ifTrue: [ ^self].

	self prChangeEventsFromLinker: theLinker side: #one. 
	self inverse isNil ifFalse: [ 
		self inverse prChangeEventsFromLinker: theLinker side: #other
	]! !

!CODERelationship publicMethodsFor: 'TRF-deletion'!

disconnectForDelete: theObject

	| anInverse anExistingObject aResult |

	self computationKind = self class computationKindAlways ifTrue: [ ^nil].

	anExistingObject := self getObjectRelationshipValue: theObject.
	 anExistingObject isNil ifTrue: [ ^nil].

	self isMultiplicityMany ifTrue: [
		anExistingObject isEmpty ifTrue: [ ^nil]
	].

	anInverse := self inverse.

	self isAggregation ifTrue: [ 
		self isMultiplicityMany 
			ifTrue: [ 
				anExistingObject do: [:anObject |
					 aResult := anObject metaInfo deleteObject: anObject
				]	
			]
			ifFalse: [ aResult := anExistingObject metaInfo deleteObject: anExistingObject].
		^aResult
	].


	(anInverse notNil and: [ anInverse isMultiplicityMany]) ifTrue: [ 
		aResult := CMGenericLinkMaker unlinkerFrom: anExistingObject to: theObject metaInfo: anInverse.
		anInverse isSynthetizedRelationship ifTrue: [ anInverse synthetizedInvalidateObject: anExistingObject].
		anInverse isSynthesisRelationship ifTrue: [ anInverse synthesisInvalidateObject: anExistingObject].
		^aResult
	].


	self isMultiplicityMany 
		ifTrue: [ 
			anExistingObject do: [:anObject |
				aResult := CMGenericLinkMaker unlinkerFrom: theObject to: anObject metaInfo: self
			]	
		]
		ifFalse: [ 
			aResult := CMGenericLinkMaker unlinkerFrom: theObject to: anExistingObject metaInfo: self
		].

	self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
	self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].

	anInverse notNil  ifTrue: [ 
		anInverse isSynthetizedRelationship ifTrue: [ anInverse synthetizedInvalidateObject: anExistingObject].
		anInverse isSynthesisRelationship ifTrue: [ anInverse synthesisInvalidateObject: anExistingObject].
	].

	^aResult!

disconnectForDeleteTC: theObject
	(self objectInstanceHasFeature: theObject)   ifFalse: [ ^nil].

	^self disconnectForDelete: theObject! !

!CODERelationship publicMethodsFor: 'TRF-derivations'!

object: theObject derive: theDerivationPath clone: theClone
	
	| anObject aMetaInfo aRelatedType |
	theObject isNil ifTrue: [ ^nil].
	theDerivationPath isNil ifTrue: [ ^nil].

	self isMultiplicityMany ifTrue: [ 
		^self object: theObject deriveMany: theDerivationPath clone: theClone
	].

	anObject := self getObjectFeatureValueTC: theObject.
	anObject isNil ifTrue: [ ^nil].

	aRelatedType := self relatedType.
	aRelatedType isNil ifTrue: [ ^nil].

	(aRelatedType isTypeOfObjectInstance: anObject) ifFalse: [ ^nil].

	theDerivationPath isEmpty ifTrue: [ 
		^Array with: (theClone == true
			ifFalse: [ anObject] 
			ifTrue: [ aRelatedType cloneObject: anObject])
	].


	aMetaInfo := anObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	^aMetaInfo object: anObject derive: theDerivationPath clone: theClone!

object: theObject deriveMany: theDerivationPath clone: theClone
	
	|  someObjects someAllDerived |
	theObject isNil ifTrue: [ ^nil].
	theDerivationPath isNil ifTrue: [ ^nil].

	self isMultiplicityMany ifFalse: [ ^nil].

	someObjects := self getObjectFeatureValueTC: theObject.
	(someObjects isNil or: [ someObjects isEmpty]) ifTrue: [ ^nil].

	someAllDerived := OrderedCollection new: someObjects size * 3.

	someObjects do: [:anObject | | anObjectMetaInfo someDerived |
		anObjectMetaInfo := anObject metaInfo.
		anObjectMetaInfo isNil ifFalse: [ 
			someDerived := anObjectMetaInfo object: anObject derive: theDerivationPath clone: theClone.
			(someDerived isNil not and: [ someDerived isEmpty not ]) ifTrue: [ 
				someAllDerived addAll: someDerived
			]
		]
	].

	^someAllDerived! !

!CODERelationship publicMethodsFor: 'TRF-dynamic metainfo'!

candidatesFeatureNameForRequiredReferencedFeatureFromPrefixes: thePrefixes

	| aNamePrefix aStream aCandidatePath aCandidatesPath someStrings |

	aCandidatesPath := self candidatesPath.
	(aCandidatesPath isNil or: [ aCandidatesPath isEmpty]) ifTrue:  [ ^nil].

	someStrings := aCandidatesPath asArrayOfSubstrings.
	someStrings size > 1 ifFalse: [ ^nil].

	aNamePrefix := (thePrefixes isNil not and: [ thePrefixes isEmpty not]) 
		ifFalse: [ '']
		ifTrue: [ 
			aStream := WriteStream on: (String new: thePrefixes size * 32).
			(thePrefixes copyFrom: 1 to: thePrefixes size) do: [:aFeature | 
				aStream nextPutAll: aFeature name; nextPutAll: '-'
			].
			aStream contents
		].

	aCandidatePath := 	aNamePrefix,  self name, self  class candidatesFeatureNameForRequiredReferencedFeaturePostfix.
	^aCandidatePath!

candidatesPathForRequiredReferencedFeatureFromPrefixes: thePrefixes

	| aCandidatePath aCandidatesPath someStrings anAllowCreate aCandidatesFeatureName |
 
	aCandidatesPath := self candidatesPath.
	(aCandidatesPath isNil or: [ aCandidatesPath isEmpty]) ifTrue:  [ ^nil].

	someStrings := aCandidatesPath asArrayOfSubstrings.
	someStrings size > 1 ifFalse: [ ^nil].

	anAllowCreate := someStrings first 	asSymbol =  self class candidatesPathCreate.

	aCandidatesFeatureName := self candidatesFeatureNameForRequiredReferencedFeatureFromPrefixes: thePrefixes.
	(aCandidatesFeatureName isNil or: [ aCandidatesFeatureName isEmpty]) ifTrue: [ ^nil].

	aCandidatePath := (anAllowCreate  ifTrue: [self class candidatesPathCreate]  ifFalse: [self class candidatesPathNoCreate]) , ' ', 
		aCandidatesFeatureName.
	^aCandidatePath!

createNonPersistentNLSFor: theSpecializedElement  originals: theOriginalTranslationElements translation: theTranslation

	| aNewTranslationItem aModel aTranslation |

	theSpecializedElement isNil ifTrue: [ ^nil].
	(theOriginalTranslationElements isNil or: [ theOriginalTranslationElements isEmpty]) ifTrue: [ ^nil].


	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aTranslation := aModel nlsSolver.
	(aTranslation isNil or: [ aTranslation isVoidTranslation]) ifTrue: [ ^nil].

	(theOriginalTranslationElements reject: [:aOriginalTranslationElement |  
		(aTranslation  
			nlsFirstResolverItemGroupNoDefault: aOriginalTranslationElement nameNLSGroupName 
			item: aOriginalTranslationElement nameNLSItemName) isNil
	]) isEmpty ifTrue: [ ^nil].

	(aTranslation nlsLocalResolverItemGroupNoDefault: theSpecializedElement nameNLSGroupName 
		item: theSpecializedElement nameNLSItemName) isNil ifTrue: [ 

		(theTranslation isNil or: [ theTranslation isEmpty])
			ifFalse: [ 
				aTranslation 
					recordNonPersistentNLSGroup: theSpecializedElement nameNLSGroupName 
					item: theSpecializedElement nameNLSItemName
					translation: theTranslation
			]
			ifTrue: [ 
				aTranslation 
					recordNonPersistentNLSGroup: theSpecializedElement nameNLSGroupName 
					item: theSpecializedElement nameNLSItemName
			].

		aNewTranslationItem := aTranslation nlsLocalResolverItemGroupNoDefault: theSpecializedElement  nameNLSGroupName
			item: theSpecializedElement nameNLSItemName.
		aNewTranslationItem isNil ifFalse: [
			theOriginalTranslationElements do: [:aOriginalTranslationElement |  | aOriginalTranslationItem |
				aOriginalTranslationItem := aTranslation  
					nlsFirstResolverItemGroupNoDefault: aOriginalTranslationElement nameNLSGroupName 
					item: aOriginalTranslationElement nameNLSItemName.
				aOriginalTranslationItem isNil ifFalse: [
					aNewTranslationItem usedItemTranslationsAdd: aOriginalTranslationItem
				]
			]
		]
	]!

initializationExpression2ForCandidatesForRequiredReferencedFeatureFromPrefixes: thePrefixes

	| aCandidatesPath someStrings somePathSteps aStream anInitializationExpression |

	aCandidatesPath := self candidatesPath.
	(aCandidatesPath isNil or: [ aCandidatesPath isEmpty]) ifTrue:  [ ^nil].

	someStrings := aCandidatesPath asArrayOfSubstrings.
	someStrings size > 1 ifFalse: [ ^nil].
	
	(someStrings at: 2)  = '2'  ifFalse: [ ^nil].

	somePathSteps :=  someStrings copyFrom: 3 to: someStrings size.

	aStream := WriteStream on: (String new: somePathSteps size * 32).
	aStream nextPutAll: self class initExpressionCloneFalse.
	somePathSteps do: [:aStep | aStream space; nextPutAll: aStep].

	anInitializationExpression := aStream contents.
	^anInitializationExpression!

initializationExpressionForCandidatesForRequiredReferencedFeatureFromPrefixes: thePrefixes

	| aCandidatesPath someStrings somePathSteps aStream anInitializationExpression |

	aCandidatesPath := self candidatesPath.
	(aCandidatesPath isNil or: [ aCandidatesPath isEmpty]) ifTrue:  [ ^nil].

	someStrings := aCandidatesPath asArrayOfSubstrings.
	someStrings size > 1 ifFalse: [ ^nil].
	
	(someStrings at: 2) = '2'  ifTrue: [
		^self initializationExpression2ForCandidatesForRequiredReferencedFeatureFromPrefixes: thePrefixes
	].


	someStrings first asSymbol = self class initExpressionSelf ifFalse: [ 
		   (someStrings at: 2) asSymbol = self class initExpressionExtent ifFalse: [ 
			(someStrings size - 3) >= thePrefixes size ifFalse: [ 
				^nil
			]
		]
	].

	somePathSteps := someStrings first asSymbol = self class initExpressionSelf 
		ifFalse: [ 
			(someStrings at: 2) asSymbol = self class initExpressionExtent  
				ifTrue: [ ^self class initExpressionExtent ]
				ifFalse: [ 
					(someStrings at: 2) asSymbol = self class initExpressionCalc 
						ifTrue: [ 
							(Array with: self class initExpressionCalc), (Array with: self class requiredFeaturesParentAttributeName) , 
								(someStrings copyFrom: thePrefixes size + 4 to: someStrings size).
						]
						ifFalse: [ 
							(Array with: self class requiredFeaturesParentAttributeName), 
							(someStrings copyFrom: thePrefixes size + 3 to: someStrings size).
						]
				]
		]
		ifTrue: [  someStrings copyFrom: 3 to: someStrings size].

	aStream := WriteStream on: (String new: somePathSteps size * 32).
	aStream nextPutAll: self class initExpressionCloneFalse.

	  
	somePathSteps do: [:aStep | aStream space; nextPutAll: aStep].

	anInitializationExpression := aStream contents.
	^anInitializationExpression!

initializationExpressionForRequiredReferencedFeatureFromPrefixes: thePrefixes

	| aInitializationExpression  someStrings somePathSteps aStream anInitializationExpression |
 
	aInitializationExpression := self initializationExpression.
	(aInitializationExpression isNil or: [ aInitializationExpression isEmpty]) ifTrue:  [ ^nil].

	someStrings := aInitializationExpression asArrayOfSubstrings.
	someStrings size > 1 ifFalse: [ ^nil].

	someStrings first asSymbol = self class initExpressionSelf ifFalse: [ 
		   (someStrings at: 2) asSymbol = self class initExpressionExtent ifFalse: [ 
			(someStrings size - 3) >= thePrefixes size ifFalse: [ 
				^nil
			]
		]
	].

	somePathSteps := someStrings first asSymbol = self class initExpressionSelf 
		ifFalse: [ 
			 (someStrings at: 2) asSymbol = self class initExpressionExtent 
				ifTrue: [ self halt: 'No idea what this does'. Array with: self class initExpressionExtent ]
				ifFalse: [ 
					(Array with: self class requiredFeaturesParentAttributeName), 
						(someStrings copyFrom: thePrefixes size + 3 to: someStrings size).
				]
		]
		ifTrue: [  someStrings copyFrom: 3 to: someStrings size].

	aStream := WriteStream on: (String new: somePathSteps size * 32).
	aStream nextPutAll: self class initExpressionCloneFalse.
	somePathSteps do: [:aStep | aStream space; nextPutAll: aStep].

	anInitializationExpression := aStream contents.
	^anInitializationExpression!

requiredCandidatesFeatureFor: theRequiredAttribute intoTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes

	| someAspects aStream aTranslation aCandidatesPath aCandidatesAttributeName aCandidatesAttribute anInitializationExpression aCandidatesConstraint |

	theRequiredAttribute isNil ifTrue: [ ^self].
	theRequiredFeaturesType isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].
	self isReferences ifFalse: [ ^self].

	self computationKind = self class computationKindInitializedInConstructor ifFalse: [ ^self].

	aCandidatesPath := self candidatesPathForRequiredReferencedFeatureFromPrefixes: thePrefixes.
	(aCandidatesPath isNil or: [ aCandidatesPath isEmpty]) ifTrue:  [ ^self].

	anInitializationExpression := self initializationExpressionForCandidatesForRequiredReferencedFeatureFromPrefixes: thePrefixes.
	(anInitializationExpression isNil or: [ anInitializationExpression isEmpty]) ifTrue:  [ ^self].

	aCandidatesConstraint := self candidatesConstraint copy.

	theRequiredAttribute candidatesPath: 		aCandidatesPath.
	theRequiredAttribute candidatesConstraint: 	
		(anInitializationExpression   asSymbol = self class initExpressionExtent ifTrue: [ String new] ifFalse: [ aCandidatesConstraint]).

	aCandidatesAttributeName := self candidatesFeatureNameForRequiredReferencedFeatureFromPrefixes: thePrefixes .
	aCandidatesAttribute := self preferredAttributeClass new.
	aCandidatesAttribute name: aCandidatesAttributeName.
	aCandidatesAttribute isNotPersistentMetaInfo: true.
	aCandidatesAttribute isInitializationPropagationAllowed: false.
	aCandidatesAttribute isInitializationPropagationOnConnectAllowed: false.

	aCandidatesAttribute minMult: self class minMultOptional.
	aCandidatesAttribute maxMult: self class maxMultMany.
	aCandidatesAttribute valueType: theRequiredAttribute valueType.
	aCandidatesAttribute isChangeable: true.
	aCandidatesAttribute computationKind: 
		(anInitializationExpression = self class initExpressionExtent ifTrue: [  aCandidatesAttribute class computationKindNoComputation] ifFalse: [  aCandidatesAttribute class computationKindAlways]).
	aCandidatesAttribute initializationExpression: anInitializationExpression.

	theRequiredFeaturesType attributesAdd: aCandidatesAttribute.


	someAspects := self aspects.
	(someAspects isNil or: [ someAspects isEmpty]) ifFalse: [ 
		someAspects do: [:anAspect |  aCandidatesAttribute aspectsAdd: anAspect]
	].
	aStream := WriteStream on: (String new: thePrefixes size * 32).
	1 to: thePrefixes size do: [:anIndex | 
		anIndex = 1 ifFalse: [ aStream nextPutAll: '.'].
		aStream nextPutAll: '%'; print: anIndex
	].

	aTranslation := aStream contents.
	self createNonPersistentNLSFor: aCandidatesAttribute  originals: thePrefixes asArray  translation: aTranslation.!

requiredFeatureNameFromPrefixes: thePrefixes

	| aNamePrefix aStream aNewRequiredAttributeName |

	aNamePrefix := thePrefixes isEmpty
		ifTrue: [ '']
		ifFalse: [ 
			aStream := WriteStream on: (String new: thePrefixes size * 32).
			(thePrefixes copyFrom: 1 to: thePrefixes size) do: [:aFeature | 
				aFeature == thePrefixes first ifFalse: [ aStream nextPutAll: '.'].
				aStream nextPutAll: aFeature name
			].
			aStream contents, ' '
		].

	aNewRequiredAttributeName := aNamePrefix,  self name.
	^aNewRequiredAttributeName!

requiredFeaturesTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes

	| somePrefixes aRelatedType |
	theRequiredFeaturesType isNil ifTrue: [ ^self].


	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].

	self isReferences ifTrue: [ 
		^self requiredReferencedFeaturesTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes
	].

	self computationKind = self class computationKindInitializedInConstructor ifFalse: [ ^self].


	aRelatedType := self relatedType.
	aRelatedType isNil ifTrue: [ ^self].
	
	somePrefixes := thePrefixes , (Array with: self).

	aRelatedType requiredFeaturesTypeMetaInfo: theRequiredFeaturesType  prefixes: somePrefixes!

requiredReferencedFeaturesTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes

	| aRelatedType aNewRequiredAttributeName aNewRequiredAttribute someAspects aStream aTranslation anInitializationExpression |

	theRequiredFeaturesType isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].
	self isReferences ifFalse: [ ^self].

	self computationKind = self class computationKindInitializedInConstructor ifFalse: [ ^self].

	aRelatedType := self relatedType.
	aRelatedType isNil ifTrue: [ ^self].

	anInitializationExpression := self initializationExpressionForRequiredReferencedFeatureFromPrefixes: thePrefixes.

	aNewRequiredAttributeName := self requiredFeatureNameFromPrefixes: thePrefixes.
	aNewRequiredAttribute := self preferredAttributeClass new.
	aNewRequiredAttribute name: aNewRequiredAttributeName.
	aNewRequiredAttribute isNotPersistentMetaInfo: true.
	aNewRequiredAttribute isInitializationPropagationAllowed: false.
	aNewRequiredAttribute isInitializationPropagationOnConnectAllowed: false.

	aNewRequiredAttribute minMult: self minMult copy.
	aNewRequiredAttribute maxMult: self class maxMultOne.
	aNewRequiredAttribute isAbstract: false.
	aNewRequiredAttribute valueType: aRelatedType.
	aNewRequiredAttribute isChangeable: true.
	aNewRequiredAttribute computationKind: aNewRequiredAttribute class computationKindInitialValue.
	aNewRequiredAttribute initializationExpression: anInitializationExpression. "self class initExpressionNilValue"
	theRequiredFeaturesType attributesAdd: aNewRequiredAttribute.

	someAspects := self aspects.
	(someAspects isNil or: [ someAspects isEmpty]) ifFalse: [ 
		someAspects do: [:anAspect |  aNewRequiredAttribute aspectsAdd: anAspect]
	].
	aStream := WriteStream on: (String new: thePrefixes size * 32).
	1 to: thePrefixes size do: [:anIndex | 
		anIndex = 1 ifFalse: [ aStream nextPutAll: '.'].
		aStream nextPutAll: '%'; print: anIndex
	].

	aTranslation := aStream contents.
	self createNonPersistentNLSFor: aNewRequiredAttribute  originals: thePrefixes asArray  translation: aTranslation.


	self requiredCandidatesFeatureFor: aNewRequiredAttribute intoTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes.!

xinitializationExpressionForCandidatesForRequiredReferencedFeatureFromPrefixes: thePrefixes

	| aCandidatesPath someStrings somePathSteps aStream anInitializationExpression |

	aCandidatesPath := self candidatesPath.
	(aCandidatesPath isNil or: [ aCandidatesPath isEmpty]) ifTrue:  [ ^nil].
self halt.
	someStrings := aCandidatesPath asArrayOfSubstrings.
	someStrings size > 1 ifFalse: [ ^nil].
	
	someStrings first asSymbol  =  self class initExpressionCalc  ifTrue: [
		^self initializationExpressionForCalcCandidatesForRequiredReferencedFeatureFromPrefixes: thePrefixes
	].


	someStrings first asSymbol = self class initExpressionSelf ifFalse: [ 
		   (someStrings at: 2) asSymbol = self class initExpressionExtent ifFalse: [ 
			(someStrings size - 3) >= thePrefixes size ifFalse: [ 
				^nil
			]
		]
	].

	somePathSteps := someStrings first asSymbol = self class initExpressionSelf 
		ifFalse: [ 
			(someStrings at: 2) asSymbol = self class initExpressionExtent  
				ifTrue: [ ^self class initExpressionExtent ]
				ifFalse: [ 
					(Array with: self class requiredFeaturesParentAttributeName), 
						(someStrings copyFrom: thePrefixes size + 3 to: someStrings size).
				]
		]
		ifTrue: [  someStrings copyFrom: 3 to: someStrings size].

	aStream := WriteStream on: (String new: somePathSteps size * 32).
	aStream nextPutAll: self class initExpressionCloneFalse.
	somePathSteps do: [:aStep | aStream space; nextPutAll: aStep].

	anInitializationExpression := aStream contents.
	^anInitializationExpression! !

!CODERelationship publicMethodsFor: 'TRF-initialization'!

initializeNewObjectFeature: theNewObject
	
	theNewObject isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].
	self isAggregation ifFalse: [ ^self].

	self computationKind = self class computationKindNoComputation 
		ifTrue: [ 
			self minMult = self class minMultRequired ifTrue: [ 
				self initializeNewObjectCreatedFeature: theNewObject
			]
		]
		ifFalse: [ 
			self computationKind = self class computationKindInitialValue 
				ifTrue: [  self initializeNewObjectComputedFeature: theNewObject]
		]!

initializeNewObjectRequiredFeature: theNewObject
	
	| aNewObject aRelatedType |
	theNewObject isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self isAggregation ifFalse: [ ^self].

	self maxMult = self class maxMultNone ifTrue: [ ^self].

	self minMult = self class minMultOptional ifTrue: [ ^self].

	aRelatedType := self relatedType.
	aRelatedType isNil ifTrue: [ ^self].

	aNewObject := self initializeNewObjectRequiredFeatureFromInitializationExpression: theNewObject.
	aNewObject isNil ifTrue: [ 
		aNewObject :=   aRelatedType createObject.
	].
	aNewObject isNil ifTrue: [ ^self].

	self isMultiplicityMany
		ifFalse: [ 	self object: theNewObject setTC: aNewObject]
		ifTrue: [ self object: theNewObject addTC: aNewObject]!

initializeNewObjectRequiredFeature: theNewObject
	withRequiredFeaturesObject: theRequiredFeaturesObject
	prefixes: thePrefixes
	
	| aNewObject aRelatedType |
	theNewObject isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].

	self maxMult = self class maxMultNone ifTrue: [ ^self].

	self isReferences ifTrue: [ 
		^self initializeNewObjectRequiredReferencedFeature: theNewObject
			withRequiredFeaturesObject: theRequiredFeaturesObject
			prefixes: thePrefixes
	].

	self isAggregation ifFalse: [ ^self].

	self computationKind = self class computationKindInitializedInConstructor ifFalse: [ ^self].

	aRelatedType := self relatedType.
	aRelatedType isNil ifTrue: [ ^self].

	aNewObject := aRelatedType createObjectWithRequiredFeaturesObject: theRequiredFeaturesObject
		prefixes: thePrefixes, (Array with: self).

	self isMultiplicityMany
		ifFalse: [ 	self object: theNewObject setTC: aNewObject]
		ifTrue: [ self object: theNewObject addTC: aNewObject]!

initializeNewObjectRequiredReferencedFeature: theNewObject
	withRequiredFeaturesObject: theRequiredFeaturesObject
	prefixes: thePrefixes
	
	| aRequiredFeaturesMetaInfo aRequiredFeatureName aRequiredFeature aRequiredValue aProvidedValue |
	theNewObject isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].
	self isReferences ifFalse: [ ^self].

	self computationKind = self class computationKindInitializedInConstructor ifFalse: [ ^self].

	aRequiredFeaturesMetaInfo := theRequiredFeaturesObject metaInfo.
	aRequiredFeaturesMetaInfo isNil ifTrue:  [ ^self].

	aRequiredFeatureName := self requiredFeatureNameFromPrefixes: thePrefixes.
	aRequiredFeature := aRequiredFeaturesMetaInfo featureOrInheritedNamed: aRequiredFeatureName.
	aRequiredFeature isNil ifTrue:  [  ^self].

	aRequiredValue := aRequiredFeature getObjectFeatureValueTC: theRequiredFeaturesObject.
	aRequiredValue isNil ifTrue: [ ^self].

	aProvidedValue := aRequiredFeature isMultiplicityMany 
		ifFalse: [ aRequiredValue]
		ifTrue: [ 
			aRequiredValue isEmpty
				ifTrue: [ nil]
				ifFalse: [ aRequiredValue first ]
		].
	

	self isMultiplicityMany
		ifFalse: [ 	self object: theNewObject setTC: aProvidedValue]
		ifTrue: [ self object: theNewObject addTC: aProvidedValue]! !

!CODERelationship publicMethodsFor: 'TRF-initialization-connect'!

initialize: theNewObject afterConnectionTo: theObject

	| aRelatedObjects  otherRelatedMetaInfo aThereWasInitialization aThereWasInitializationInRelated |
	theNewObject isNil ifTrue: [ ^false].
	theObject isNil ifTrue: [ ^false].

	aThereWasInitialization := self initializeConnectedObjectComputedFeature: theNewObject afterConnectionTo: theObject.
 
	self isAggregation ifTrue: [ 
		aRelatedObjects := self getObjectFeatureValueTC: theNewObject.
		self isMultiplicityMany
			ifTrue: [ 
				aRelatedObjects isEmpty ifFalse: [ 
					aRelatedObjects do: [:aRelatedObject | | aRelatedMetaInfo aThereWasInitInRel |
						(aRelatedObject isKindOf: CMGenericObject) ifTrue: [ 
							aRelatedMetaInfo := aRelatedObject metaInfo.
							aRelatedMetaInfo isNil ifFalse: [ 
								aThereWasInitInRel := aRelatedMetaInfo initialize: aRelatedObject afterConnectionTo: theNewObject.
								aThereWasInitialization  := aThereWasInitialization or: [ aThereWasInitInRel].
							]
						]
					]
				]
			]
			ifFalse: [ 
				aRelatedObjects isNil ifFalse: [ 
					otherRelatedMetaInfo := aRelatedObjects metaInfo.
					otherRelatedMetaInfo isNil ifFalse: [ 
						aThereWasInitializationInRelated := otherRelatedMetaInfo initialize: aRelatedObjects afterConnectionTo: theNewObject.
						aThereWasInitialization  := aThereWasInitialization or: [ aThereWasInitializationInRelated].
					]
				]
			]
	].

	^aThereWasInitialization!

initialize: theNewObject afterConnectionTo: theObject linkBlock: theBlock

	| aNewObjectMetaInfo aOwnerObjectMetaInfo aResult aRelatedType aNonVirtualRelatedType aDomain aType aHomeForIdCounter aBlock |

	theObject isNil ifTrue: [ ^nil].
	theNewObject isNil ifTrue: [ ^nil].
	theBlock isNil ifTrue: [ ^nil].

	aBlock := [ | aRes |
		(CMTransaction  newTransactionDo: [ 
			aRes := theBlock value .
			self updateObject: theObject value: theNewObject.
			self inverse updateObject: theNewObject value: theObject.
		]) 
			ifTrue: [aRes]
			ifFalse: [ nil]
	].

	self isAggregation ifFalse: [  ^aBlock value].

	theNewObject isNil ifTrue: [  ^aBlock value].

	aType := self type.
	aType isNil ifTrue: [ ^nil].

	aType isDomainType ifTrue: [ ^theBlock value].

	self isInitializationPropagationOnConnectAllowed ifFalse: [ ^aBlock value].

	aRelatedType := self relatedType.
	aRelatedType isNil ifTrue: [ ^nil].

	aNonVirtualRelatedType := aRelatedType nonVirtualType.
	aNonVirtualRelatedType isNil ifTrue: [ ^nil].

	aNewObjectMetaInfo :=  theNewObject metaInfo.
	aNewObjectMetaInfo isNil ifTrue: [ ^nil].
	

	aBlock := [ | aRes |
		(CMTransaction  newTransactionDo: [ 
			aRes := theBlock value .
			aNewObjectMetaInfo initialize: theNewObject afterConnectionTo: theObject.
			self updateObject: theObject value: theNewObject.
		]) 
			ifTrue: [aRes]
			ifFalse: [ nil]
	].


	aOwnerObjectMetaInfo :=  theObject metaInfo.
	aOwnerObjectMetaInfo isNil ifTrue: [ ^nil].
				
	aDomain := aOwnerObjectMetaInfo searchDomainFrom: theObject.
	aDomain isNil ifTrue:[ ^aBlock value].


	aBlock := [ | aRes |
		(CMTransaction  newTransactionDo: [ 
			aRes := theBlock value .
			aNewObjectMetaInfo initialize: theNewObject afterConnectionTo: theObject.
			self updateObject: theObject value: theNewObject.
			aNewObjectMetaInfo anchorNewObject: theNewObject inDomain: aDomain.
		]) 
			ifTrue: [aRes]
			ifFalse: [ nil]
	].


	aHomeForIdCounter := aNewObjectMetaInfo getHomeForIdCounterInDomain: aDomain.
	aHomeForIdCounter isNil ifTrue: [ 
		aHomeForIdCounter := aOwnerObjectMetaInfo searchHomeForIdCounterFrom: theObject inDomain: aDomain.
		aHomeForIdCounter isNil ifTrue: [ ^aBlock value]
	].

	aResult := nil.
	^(CMTransaction  newTransactionDo: [ 

		aResult := theBlock value .
		aNewObjectMetaInfo initialize: theNewObject afterConnectionTo: theObject.
		aNewObjectMetaInfo initializeNewObjectIDAttributes: theNewObject inHome: aHomeForIdCounter.
		self updateObject: theObject value: theNewObject.
		aNewObjectMetaInfo anchorNewObject: theNewObject inDomain: aDomain.

	]) 
		ifTrue: [aResult]
		ifFalse: [ nil]!

searchDomainFrom: theObject

	| anOwnerObject aOwnerMetaInfo someOwners aDomain |

	theObject isNil ifTrue: [ ^nil].

	self isAggregated ifFalse: [ ^nil].

	anOwnerObject := self getObjectFeatureValueTC: theObject.
	anOwnerObject isNil ifTrue: [ ^nil].
	self isMultiplicityMany  ifFalse: [ 
		aOwnerMetaInfo := anOwnerObject metaInfo.
		aOwnerMetaInfo isNil ifTrue: [ ^nil].
		^aOwnerMetaInfo searchDomainFrom: anOwnerObject
	].

	someOwners := anOwnerObject.
	aDomain := nil.
	someOwners detect: [:anOwner |  | aOMI |
		aOMI := anOwner metaInfo.
		aOMI isNil ifFalse: [
			aDomain := aOMI searchDomainFrom: anOwnerObject
		].
		aDomain isNil not
	] ifNone: [ nil].

	^aDomain!

searchHomeForIdCounterFrom: theObject  inDomain: theDomain

	| anOwnerObject aOwnerMetaInfo someOwners aHome |

	theObject isNil ifTrue: [ ^nil].

	self isAggregated ifFalse: [ ^nil].

	anOwnerObject := self getObjectFeatureValueTC: theObject.
	anOwnerObject isNil ifTrue: [ ^nil].
	self isMultiplicityMany  ifFalse: [ 
		aOwnerMetaInfo := anOwnerObject metaInfo.
		aOwnerMetaInfo isNil ifTrue: [ ^nil].
		^aOwnerMetaInfo searchHomeForIdCounterFrom: anOwnerObject  inDomain: theDomain
	].

	someOwners := anOwnerObject.
	aHome := nil.
	someOwners detect: [:anOwner |  | aOMI |
		aOMI := anOwner metaInfo.
		aOMI isNil ifFalse: [
			aHome := aOwnerMetaInfo searchHomeForIdCounterFrom: anOwnerObject  inDomain: theDomain
		].
		aHome isNil not
	] ifNone: [ nil].

	^aHome!

update: theOldObject afterDisconnectionFrom: theObject linkBlock: theBlock

	| aBlock |

	theObject isNil ifTrue: [ ^nil].
	theOldObject isNil ifTrue: [ ^nil].
	theBlock isNil ifTrue: [ ^nil].

	aBlock := [ | aRes |
		(CMTransaction  newTransactionDo: [ 
			aRes := theBlock value .
			self updateObject: theObject value: theOldObject.
			self inverse updateObject: theOldObject value: theObject.
		]) 
			ifTrue: [aRes]
			ifFalse: [ nil]
	].

	 ^aBlock value! !

!CODERelationship publicMethodsFor: 'TRF-object accessing'!

getObjectFeatureValue: theObject
	^self getObjectRelationshipValue: theObject!

getObjectFeatureValueTC: theObject

	^self getObjectRelationshipValueTC: theObject!

getObjectRelationshipValue: theObject
	| aResult |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].
 
	self computationKind = self class computationKindAlways ifTrue: [ 
		aResult := self  computeDerivedValueFrom: theObject expression: self initializationExpression.
		^aResult isNil 
			ifTrue: [ nil]
			ifFalse: [ 
				self isMultiplicityMany
					ifTrue: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult ]]
					ifFalse: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult asArray first]]
			]
	].

	self isSynthetizedRelationship ifTrue: [ 
		aResult := self  synthetizedValueFrom: theObject.
		^aResult "isNil 
			ifTrue: [ nil]
			ifFalse: [ 
				self isMultiplicityMany
					ifTrue: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult ]]
					ifFalse: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult asArray first]]
			]"
	].

	^self getObjectRelationshipValueStored: theObject!

getObjectRelationshipValueStored: theObject
	| aValue |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].
 
	self computationKind = self class computationKindAlways ifTrue: [ ^nil].

	aValue := theObject propertyAt: self name asSymbol.

	aValue = self class alreadyInitializedSentinelValue ifTrue: [
		^self isMultiplicityMany 
			ifFalse:[ nil]
			ifTrue: [ self isOrdered ifTrue: [ OrderedCollection new: 1] ifFalse: [ IdentitySet new: 3]]
	].
	
	aValue isNil ifFalse: [
		^self isMultiplicityMany 
			ifFalse:[ aValue]
			ifTrue: [ aValue copy]
	].

	^self isMultiplicityMany 
		ifFalse:[ nil]
		ifTrue: [ self isOrdered ifTrue: [ OrderedCollection new: 1] ifFalse: [ IdentitySet new: 3]]!

getObjectRelationshipValueTC: theObject

	(self isTypeOfObjectInstance: theObject)  ifFalse: [ ^self isMultiplicityMany ifTrue: [ Array new] ifFalse: [ nil]].

	^self getObjectRelationshipValue: theObject!

getRelatedObjectNameValueNoDefault: theObject

	| aRelatedObject anObjectMetaInfo |
	theObject isNil ifTrue: [ ^nil].

	aRelatedObject := self getObjectFeatureValueTC: theObject.
	aRelatedObject isNil ifTrue: [ ^nil].

	anObjectMetaInfo := aRelatedObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [ ^nil].
	
	^anObjectMetaInfo getObjectNameValueNoDefault: aRelatedObject! !

!CODERelationship publicMethodsFor: 'TRF-object accessing-private'!

addFirstToCollection: theCollection object: theObject 
	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	^theCollection addFirst: theObject!

addLastToCollection: theCollection object: theObject 
	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	^theCollection addLast: theObject!

addToCollection: theCollection object: theObject 
	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	^theCollection add: theObject!

addToCollection: theCollection object: theObject afterIndex: theAfterIndex

	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	theAfterIndex isNil ifTrue: [ ^self addToCollection: theCollection object: theObject].

	^theCollection add: theObject afterIndex: theAfterIndex!

addToCollection: theCollection object: theObject before: theBeforeObject

	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	theBeforeObject isNil ifTrue: [ ^self addToCollection: theCollection object: theObject].

	^theCollection add: theObject before: theBeforeObject!

addToCollection: theCollection object: theObject beforeIndex: theBeforeIndex

	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	theBeforeIndex isNil ifTrue: [ ^self addToCollection: theCollection object: theObject].

	^theCollection add: theObject beforeIndex: theBeforeIndex!

collection: theCollection includes: theObject

	theCollection isNil ifTrue: [ ^false].
	theObject isNil ifTrue: [ ^false].

	^theCollection includes: theObject!

collection: theCollection indexOf: theObject

	theCollection isNil ifTrue: [ ^false].
	theObject isNil ifTrue: [ ^false].

	^theCollection indexOf:  theObject!

getNoInitObjectRelationshipValue: theObject

	theObject isNil ifTrue: [ ^nil].

	^(theObject isKindOf: CMGenericObject) 
		ifFalse: [ theObject perform: self getSelector ]
		ifTrue: [ theObject propertyAt: self name asSymbol]!

removeFromCollection: theCollection object: theObject 
	theCollection isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	^theCollection remove: theObject ifAbsent: [ nil]!

restoreObject: theObject relationshipValue: theRestoredValue
	theObject isNil ifTrue: [ ^nil].

	^(theObject isKindOf: CMGenericObject) 
		ifTrue: [ theObject propertyAt: self name asSymbol put: theRestoredValue]
		ifFalse: [ theObject perform: self restoreSelector with: theRestoredValue]!

setObject: theObject featureValue: theNewObject
	^self setObject: theObject relationshipValue: theNewObject!

setObject: theObject relationshipValue: theNewObject
	theObject isNil ifTrue: [ ^nil].

	^(theObject isKindOf: CMGenericObject) 
		ifTrue: [ theObject propertyAt: self name asSymbol put: theNewObject]
		ifFalse: [ theObject perform: self setSelector with: theNewObject]! !

!CODERelationship publicMethodsFor: 'TRF-operations'!

object: theObject add: theNewObject 

	self isMultiplicityMany ifFalse: [ ^nil].

	^self initialize: theNewObject afterConnectionTo: theObject linkBlock: [ | aRes |
		aRes := CMGenericLinkMaker from: theObject to: theNewObject metaInfo: self.
		self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
		self inverse  isSynthetizedRelationship ifTrue: [ self inverse synthetizedInvalidateObject: theNewObject].
		self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
		self inverse  isSynthesisRelationship ifTrue: [ self inverse synthesisInvalidateObject: theNewObject].
		aRes
	]!

object: theObject add: theNewObject before: theOneBefore

	^self object: theObject before: theOneBefore add: theNewObject before: nil!

object: theObject addTC: theNewObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject )  ifFalse: [ ^nil].

	^self object: theObject add: theNewObject!

object: theObject addTC: theNewObject before: theOneBefore

	^self object: theObject before: theOneBefore addTC: theNewObject before: nil!

object: theObject before: theOneBefore add: theNewObject before: theOtherBefore

	self isMultiplicityMany ifFalse: [ ^nil].

	^self initialize: theNewObject afterConnectionTo: theObject linkBlock: [ | aRes |
		aRes := CMGenericLinkMaker from: theObject before: theOneBefore to: theNewObject before: theOtherBefore metaInfo: self.
		self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
		self inverse  isSynthetizedRelationship ifTrue: [ self inverse synthetizedInvalidateObject: theNewObject].
		self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
		self inverse  isSynthesisRelationship ifTrue: [ self inverse synthesisInvalidateObject: theNewObject].
		aRes
	
	]!

object: theObject before: theOneBefore addTC: theNewObject before: theOtherBefore

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject )  ifFalse: [ ^nil].

	^self object: theObject before: theOneBefore add: theNewObject before: theOtherBefore!

object: theObject move: theExistingObject  relative: theObjectRelativeTo

	| aRes |
	self isMultiplicityMany ifFalse: [ ^nil].

	aRes := CMGenericLinkMaker from: theObject to: theExistingObject 
		move: self class movementRelativeSymbol before: theObjectRelativeTo metaInfo: self.
	self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
	self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
	^aRes!

object: theObject moveDown: theExistingObject 

	| aRes |
	self isMultiplicityMany ifFalse: [ ^nil].

	aRes := CMGenericLinkMaker from: theObject to: theExistingObject move: self class movementDownSymbol metaInfo: self.
	self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
	self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
	^aRes!

object: theObject moveDownTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject )  ifFalse: [ ^nil].

	^self object: theObject moveDown: theExistingObject!

object: theObject moveTC: theExistingObject relative: theRelativeToObject

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject )  ifFalse: [ ^nil].

	theRelativeToObject isNil ifFalse: [ 
		(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theRelativeToObject )  ifFalse: [ ^nil].
	].

	^self object: theObject move: theExistingObject relative: theRelativeToObject!

object: theObject moveToBottom: theExistingObject 

	| aRes |
	self isMultiplicityMany ifFalse: [ ^nil].

	aRes := CMGenericLinkMaker from: theObject to: theExistingObject move: self class movementBottomSymbol metaInfo: self.

	self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
	self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
	^aRes!

object: theObject moveToBottomTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject )  ifFalse: [ ^nil].

	^self object: theObject moveToBottom: theExistingObject!

object: theObject moveToTop: theExistingObject 

	| aRes |
	self isMultiplicityMany ifFalse: [ ^nil].

	aRes := CMGenericLinkMaker from: theObject to: theExistingObject move: self class movementTopSymbol metaInfo: self.

	self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
	self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
	^aRes!

object: theObject moveToTopTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject )  ifFalse: [ ^nil].

	^self object: theObject moveToTop: theExistingObject!

object: theObject moveUp: theExistingObject 

	| aRes |
	self isMultiplicityMany ifFalse: [ ^nil].

	aRes := CMGenericLinkMaker from: theObject to: theExistingObject move: self class movementUpSymbol metaInfo: self.

	self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
	self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
	^aRes!

object: theObject moveUpTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject )  ifFalse: [ ^nil].

	^self object: theObject moveUp: theExistingObject!

object: theObject remove: theExistingObject 

	| aLinkBLock |
	self isMultiplicityMany ifFalse: [ ^nil].

	self isAggregation ifTrue: [ 
		^theExistingObject metaInfo deleteObject: theExistingObject
	].

	aLinkBLock := [ | aRes |
		aRes := CMGenericLinkMaker unlinkerFrom: theObject to: theExistingObject metaInfo: self.
		self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
		self inverse  isSynthetizedRelationship ifTrue: [ self inverse synthetizedInvalidateObject: theExistingObject].
		self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
		self inverse  isSynthesisRelationship ifTrue: [ self inverse synthesisInvalidateObject: theExistingObject].
		aRes
	].
	^self update: theExistingObject afterDisconnectionFrom: theObject linkBlock: aLinkBLock!

object: theObject removeTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject )  ifFalse: [ ^nil].

	^self object: theObject remove: theExistingObject!

object: theObject set: theNewObject 

	| anInverse |
	self isMultiplicityOne ifFalse: [ ^nil].
	
	anInverse := self inverse.
	(anInverse notNil and: [ anInverse isMultiplicityMany]) ifTrue: [ 
		^anInverse object: theNewObject add: theObject
	].

	^self initialize: theNewObject afterConnectionTo: theObject linkBlock: [  | aRes |
		aRes := CMGenericLinkMaker from: theObject to: theNewObject metaInfo: self.
		self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
		self inverse  isSynthetizedRelationship ifTrue: [ self inverse synthetizedInvalidateObject: theNewObject].
		self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
		self inverse  isSynthesisRelationship ifTrue: [ self inverse synthesisInvalidateObject: theNewObject].
		aRes

	]!

object: theObject setTC: theNewObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject )  ifFalse: [ ^nil].

	^self object: theObject set: theNewObject!

object: theObject unset: theExistingObject 

	| anInverse aLinkBLock |
	self isMultiplicityOne ifFalse: [ ^nil].

	self isAggregated  ifTrue: [  
		^theObject metaInfo deleteObject: theObject
	].

	anInverse := self inverse.
	(anInverse notNil and: [ anInverse isMultiplicityMany]) ifTrue: [ 
		^anInverse object: theExistingObject remove: theObject
	].

	aLinkBLock := [  | aRes |
		aRes := CMGenericLinkMaker unlinkerFrom: theObject to: theExistingObject metaInfo: self.
		self isSynthetizedRelationship ifTrue: [ self synthetizedInvalidateObject: theObject].
		self inverse  isSynthetizedRelationship ifTrue: [ self inverse synthetizedInvalidateObject: theExistingObject].
		self isSynthesisRelationship ifTrue: [ self synthesisInvalidateObject: theObject].
		self inverse  isSynthesisRelationship ifTrue: [ self inverse synthesisInvalidateObject: theExistingObject].
		aRes
	].
	^self update: theExistingObject afterDisconnectionFrom: theObject linkBlock: aLinkBLock!

object: theObject unsetTC: theExistingObject 

	(self objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theExistingObject )  ifFalse: [ ^nil].

	^self object: theObject unset: theExistingObject! !

!CODERelationship publicMethodsFor: 'TRF-parms'!

getSelector
	^(self reengineredClassName, 'Exact') asSymbol!

restoreSelector
	^(self reengineredClassName, 'Forced:') asSymbol!

setSelector
	^(self reengineredClassName, 'Forced:') asSymbol! !

!CODERelationship publicMethodsFor: 'TRF-subActions'!

subActionPreviousToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doMove  ifTrue: [ ^true].
	theLinker doUnlink ifTrue: [ ^true].

	^(self saPreviousToDoLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saPreviousToDoLink: theLinker side: #other]
	]!

subActionSaveToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doMove  ifTrue: [ ^self subActionSaveToDoMove: theLinker].

	^(self saSaveToDoLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saSaveToDoLink: theLinker side: #other]
	]!

subActionSaveToDoMove: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saSaveToDoMove: theLinker!

subActionTestToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doMove  ifTrue: [ ^self subActionTestToDoMove: theLinker].
	theLinker doUnlink ifTrue: [ ^self subActionTestToDoUnLink: theLinker].

	theLinker doOrdered ifFalse: [ ^true].

	^(self saTestToDoLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saTestToDoLink: theLinker side: #other]
	].!

subActionTestToDoMove: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saTestToDoMove: theLinker!

subActionTestToDoUnLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	^(self saTestToDoUnLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saTestToDoUnLink: theLinker side: #other]
	].!

subActionToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doMove  ifTrue: [ ^self subActionToDoMove: theLinker].
	theLinker doUnlink ifTrue: [ ^self subActionToDoUnLink: theLinker].

	^(self saToDoLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saToDoLink: theLinker side: #other]
	]!

subActionToDoMove: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saToDoMove: theLinker!

subActionToDoUnLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	^(self saToDoUnLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saToDoUnLink: theLinker side: #other]
	]!

subActionToUndoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doMove  ifTrue: [ ^self subActionToUndoMove: theLinker].

	^(self saToUndoLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saToUndoLink: theLinker side: #other]
	]!

subActionToUndoMove: theLinker

	theLinker isNil ifTrue: [ ^false].

	^self saToUndoMove: theLinker!

subActionUniqTestToDoLink: theLinker

	theLinker isNil ifTrue: [ ^false].

	theLinker doMove  ifTrue: [ ^true].
	theLinker doUnlink ifTrue: [ ^true].

	^(self saUniqTestToDoLink: theLinker side: #one) and: [ 
		self inverse isNil or: [ self inverse saUniqTestToDoLink: theLinker side: #other]
	]! !

!CODERelationship publicMethodsFor: 'TRF-subActions-private'!

saPreviousToDoLink: theLinker side: theSide

	| aCurrentlyLinked anObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].
	self isMultiplicityMany	ifTrue: [ ^true ].

	anObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	anObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: anObject.
	aCurrentlyLinked isNil ifTrue: [ ^true].

	^CMGenericLinkMaker unlinkerInsideFrom: anObject to: aCurrentlyLinked metaInfo: self!

saSaveToDoLink: theLinker side: theSide

	| aCurrentlyLinked anObject aValueToSave |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone ifTrue: [ ^true].

	anObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	anObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: anObject.
	aValueToSave := aCurrentlyLinked isNil 
		ifTrue: [ nil] 
		ifFalse: [ 
			self isMultiplicityMany 
				ifTrue: [ aCurrentlyLinked copy] 
				ifFalse: [ aCurrentlyLinked]
		].
		
	theLinker save: aValueToSave side: theSide.

	^true!

saSaveToDoMove: theLinker

	| aCurrentlyLinked anObject aValueToSave |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone ifTrue: [ ^true].
	self isMultiplicityMany ifFalse: [ ^true].

	anObject := theLinker oneObject.
	anObject isNil ifTrue: [ ^self error: 'Can not move without object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: anObject.
	aValueToSave := aCurrentlyLinked isNil 
		ifTrue: [ nil] 
		ifFalse: [ aCurrentlyLinked copy].
		
	theLinker save: aValueToSave side: #One.

	^true!

saTestToDoLink: theLinker side: theSide

	| aCurrentlyLinked aOneObject aOneBeforeObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].
	self isMultiplicityMany 	ifFalse: [ ^true].

	aOneObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	aOneObject isNil ifTrue: [ ^self error: 'Can not link without one object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: aOneObject.
	(aCurrentlyLinked isNil or: [ aCurrentlyLinked isEmpty])  ifTrue: [ ^false].

	aOneBeforeObject := theSide = #one ifTrue: [ theLinker oneBefore] ifFalse: [ theLinker otherBefore].
	aOneBeforeObject isNil ifTrue: [ ^true].

	^self collection: aCurrentlyLinked includes: aOneBeforeObject!

saTestToDoMove: theLinker 

	| aCurrentlyLinked aOneObject aOneBeforeObject aOtherObject aMovement |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].
	self isMultiplicityMany 	ifFalse: [ ^true].

	aOneObject := theLinker oneObject.
	aOneObject isNil ifTrue: [ ^self error: 'Can not move without one object'].

	aOtherObject := theLinker otherObject.
	aOtherObject isNil ifTrue: [ ^self error: 'Can not move without other object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: aOneObject.
	(aCurrentlyLinked isNil or: [ aCurrentlyLinked isEmpty])  ifTrue: [ ^false].

	(self collection: aCurrentlyLinked includes: aOtherObject) ifFalse: [ ^false].

	aMovement := theLinker movement.
	aMovement = self class movementRelativeSymbol ifTrue: [ 
		aOneBeforeObject := theLinker oneBefore.
		aOneBeforeObject isNil ifFalse: [ 
			(self collection: aCurrentlyLinked includes: aOneBeforeObject) ifFalse: [ ^false]
		]
	].

	^true!

saTestToDoUnLink: theLinker side: theSide

	| aCurrentlyLinked aOneObject anOtherObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone ifTrue: [ ^true].

	aOneObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	aOneObject isNil ifTrue: [ ^self error: 'Can not unlink without object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: aOneObject.
	(aCurrentlyLinked isNil or: [ aCurrentlyLinked isEmpty])  ifTrue: [ ^false].

	anOtherObject := theSide = #one ifTrue: [ theLinker otherObject] ifFalse: [ theLinker oneObject].
	anOtherObject isNil ifTrue: [ ^self error: 'Can not unlink without object'].

	^self isMultiplicityMany	
		ifTrue: [ self collection: aCurrentlyLinked includes: anOtherObject]
		ifFalse: [ aCurrentlyLinked == anOtherObject]!

saToDoLink: theLinker side: theSide

	| aCurrentlyLinked anOtherObject aOneObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	aOneObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	aOneObject isNil ifTrue: [ ^self error: 'Can not link without one object'].

	anOtherObject := theSide = #one ifTrue: [ theLinker otherObject] ifFalse: [ theLinker oneObject].
	anOtherObject isNil ifTrue: [ ^self error: 'Can not link without other object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: aOneObject.

	(aCurrentlyLinked isNil and: [ self isMultiplicityMany]) ifTrue: [ 
		aCurrentlyLinked := self isOrdered ifTrue: [ OrderedCollection new: 1] ifFalse: [ IdentitySet new: 3].
		aOneObject propertyAt: self name asSymbol put: aCurrentlyLinked.
	].

	^self isMultiplicityMany	
		ifTrue: [
			theLinker doOrdered
				ifTrue: [  self addToCollection: aCurrentlyLinked object: anOtherObject before: theLinker beforeObject]
				ifFalse: [  self addToCollection: aCurrentlyLinked object: anOtherObject ].
			true
		]
		ifFalse: [ 
			aCurrentlyLinked isNil
				ifFalse: [ false]
				ifTrue: [ 
					self setObject: aOneObject relationshipValue: anOtherObject.
					true
				]
		]!

saToDoMove: theLinker 

	| aCurrentlyLinked anOtherObject aOneObject aMovement aOneBeforeObject anIndex |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	aOneObject := theLinker oneObject.
	aOneObject isNil ifTrue: [ ^self error: 'Can not move without object'].

	anOtherObject := theLinker otherObject.
	anOtherObject isNil ifTrue: [ ^self error: 'Can not move without other object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: aOneObject.

	aCurrentlyLinked isNil ifTrue: [ ^false].

	anIndex := self collection: aCurrentlyLinked indexOf: anOtherObject.
	anIndex < 1 ifTrue: [ ^false].

	aCurrentlyLinked remove: anOtherObject.

	aMovement := theLinker movement.
	aMovement = self class movementRelativeSymbol ifTrue: [ 
		aOneBeforeObject := theLinker oneBefore.
		^aOneBeforeObject isNil 
			ifTrue: [  
				aCurrentlyLinked addLast: anOtherObject.
				true
			]
			ifFalse: [ 
				aOneBeforeObject == anOtherObject 
					ifTrue: [ true]
					ifFalse: [ 
						(aCurrentlyLinked includes: aOneBeforeObject) 
							ifTrue: [ self addToCollection: aCurrentlyLinked object: anOtherObject before: aOneBeforeObject]
							ifFalse: [ self addToCollection: aCurrentlyLinked object: anOtherObject].
						true
					]
			]
	].

	aMovement = self class movementTopSymbol ifTrue: [ 
		anOtherObject == aCurrentlyLinked first ifTrue: [ ^true].
		self addFirstToCollection: aCurrentlyLinked object: anOtherObject.
		^true
	].
	aMovement = self class movementBottomSymbol ifTrue: [ 
		anOtherObject == aCurrentlyLinked last ifTrue: [ ^true].
		self addLastToCollection: aCurrentlyLinked object: anOtherObject.
		^true
	].

	aMovement = self class movementUpSymbol ifTrue: [ 
		anOtherObject == aCurrentlyLinked first ifTrue: [ ^true].
		self addToCollection: aCurrentlyLinked object: anOtherObject beforeIndex: anIndex - 1.
		^true
	].

	aMovement = self class movementDownSymbol ifTrue: [ 
		anOtherObject == aCurrentlyLinked first ifTrue: [ ^true].
		self addToCollection: aCurrentlyLinked object: anOtherObject afterIndex: anIndex.
		^true
	].

	^false!

saToDoUnLink: theLinker side: theSide

	| aCurrentlyLinked anOtherObject aOneObject |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	aOneObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	aOneObject isNil ifTrue: [ ^self error: 'Can not unlink without object'].

	anOtherObject := theSide = #one ifTrue: [ theLinker otherObject] ifFalse: [ theLinker oneObject].
	anOtherObject isNil ifTrue: [ ^self error: 'Can not unlink without object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: aOneObject.
	(aCurrentlyLinked isNil or: [ aCurrentlyLinked isEmpty])  ifTrue: [ ^false].

	^self isMultiplicityMany	
		ifTrue: [ 
			(self collection: aCurrentlyLinked includes: anOtherObject)
				ifFalse: [ false]
				ifTrue: [ 
					self removeFromCollection: aCurrentlyLinked object: anOtherObject.
					true
				]
		]
		ifFalse: [ 
			aCurrentlyLinked == anOtherObject 
				ifFalse: [ false]
				ifTrue: [ 
					self setObject: aOneObject relationshipValue: nil.
					true
				]
		]!

saToUndoLink: theLinker side: theSide

	| anObject aSaved |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].

	anObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	anObject isNil ifTrue: [ ^self error: 'Can not undo without object'].

	aSaved := theSide = #one ifTrue: [ theLinker oneSaved] ifFalse: [ theLinker otherSaved].

	self restoreObject: anObject relationshipValue: aSaved.
	
	^true!

saToUndoMove: theLinker

	| anObject aSaved |

	theLinker isNil ifTrue: [ ^false].

	self isMultiplicityNone 	ifTrue: [ ^true].
	self isMultiplicityMany 	ifFalse: [ ^true].

	anObject := theLinker oneObject.
	anObject isNil ifTrue: [ ^self error: 'Can not undo without one object'].

	aSaved := theLinker oneSaved.

	self restoreObject: anObject relationshipValue: aSaved.
	
	^true!

saUniqTestToDoLink: theLinker side: theSide

	| aCurrentlyLinked aOneObject anOtherObject |

	theLinker isNil ifTrue: [ ^false].

	theLinker doUnlink ifTrue: [ ^self subActionPreviousToDoUnLink: theLinker].

	self isMultiplicityNone ifTrue: [ ^true].
	self isMultiplicityOne ifTrue: [ ^true ].
	self isDuplicatesAllowed ifTrue: [ ^true].

	aOneObject := theSide = #one ifTrue: [ theLinker oneObject] ifFalse: [ theLinker otherObject].
	aOneObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	aCurrentlyLinked := self getNoInitObjectRelationshipValue: aOneObject.
	(aCurrentlyLinked isNil or: [ aCurrentlyLinked isEmpty])  ifTrue: [ ^true].

	anOtherObject := theSide = #one ifTrue: [ theLinker otherObject] ifFalse: [ theLinker oneObject].
	anOtherObject isNil ifTrue: [ ^self error: 'Can not link without object'].

	^(aCurrentlyLinked includes:  anOtherObject) not! !

!CODERelationship publicMethodsFor: 'TRF-synthesis'!

computeSynthetizedOverridenSingleValueFrom: theObject 
	synthesisRelationship: 				theSynthesisRelationship 
	synthetizedValueInto: 					theSynthetizedValue 

	| someBaseObjects aIsMultiplicityMany aStoredValue |

	theObject isNil ifTrue: [ ^self].
	theSynthetizedValue isNil ifTrue: [ ^self].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].

 	self isSynthetizedRelationship ifFalse: [ ^self].
	self isPropagateSynthesisRelationship ifFalse: [ ^self].	

	aIsMultiplicityMany := self isMultiplicityMany.
	aIsMultiplicityMany ifTrue: [ ^self].
	
	aStoredValue := self getObjectRelationshipValueStored: theObject.
	aStoredValue isNil ifFalse: [ 
		theSynthetizedValue add: aStoredValue.
		^self
	].

	someBaseObjects := theSynthesisRelationship getObjectRelationshipValue: theObject.
	(someBaseObjects isNil not and: [  someBaseObjects isEmpty not]) ifTrue: [ 
		someBaseObjects detect: [:aBaseObject |  | aBaseObjectSynthetizedRelationship |
			aBaseObjectSynthetizedRelationship := self  sameOrEquivalentRelationshipOn: aBaseObject.
			aBaseObjectSynthetizedRelationship isNil not and: [ 
				aBaseObjectSynthetizedRelationship
					computeSynthetizedOverridenSingleValueFrom: aBaseObject 
					synthesisRelationship: 					theSynthesisRelationship 
					synthetizedValueInto: 					theSynthetizedValue. 
				theSynthetizedValue isEmpty not
			]
		] ifNone: [ nil].
	].!

computeSynthetizedOverridenValueFrom: theObject
	| aSynthesisCache aSynthetizedValue aCachedSynthetizedValue aMetaInfo aSynthesisRelationship aRelatedTypeMetaInfo aRelatedTypeSynthesisRelationship aNotToIncludeSet aIsMultiplicityMany aRelatedValue |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].
 	self isSynthetizedRelationship ifFalse: [ ^nil].

	self isPropagateSynthesisRelationship ifFalse: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].
	
	aSynthesisRelationship := aMetaInfo synthesisRelationship.
	aSynthesisRelationship isNil ifTrue: [ ^nil].

	aRelatedTypeMetaInfo := self relatedType.
	aRelatedTypeMetaInfo isNil ifTrue: [ ^nil].

	aRelatedTypeSynthesisRelationship  := aRelatedTypeMetaInfo synthesisRelationship.
	aRelatedTypeSynthesisRelationship isNil ifTrue: [ ^nil].
	
	aIsMultiplicityMany := self isMultiplicityMany.

	aSynthetizedValue := aIsMultiplicityMany ifTrue: [ self isOrdered ifTrue: [ OrderedCollection new: 8] ifFalse: [ IdentitySet new: 13]] ifFalse: [ OrderedCollection new: 1].
	
	aIsMultiplicityMany
		ifFalse: [ 
			self computeSynthetizedOverridenSingleValueFrom: theObject 
				synthesisRelationship: 				aSynthesisRelationship 
				synthetizedValueInto: 				aSynthetizedValue 
		]
		ifTrue: [ 
			aNotToIncludeSet := IdentitySet new: 13.
			self computeSynthetizedOverridenValueFrom: theObject 
				synthesisRelationship: 				aSynthesisRelationship 
				relatedTypeSynthesisRelationship: 	aRelatedTypeSynthesisRelationship 
				synthetizedValueInto: 				aSynthetizedValue 
				notToIncludeInto: 					aNotToIncludeSet.
		].


	aSynthesisCache := theObject synthesisCache.
	aSynthesisCache isNil ifTrue: [ 
		aSynthesisCache := theObject initSynthesisCache
	].
	
	aRelatedValue := aIsMultiplicityMany 
		ifTrue: [ aSynthetizedValue] 
		ifFalse: [ 
			aSynthetizedValue isNil 
				ifTrue: [ nil] 
				ifFalse: [ 
					aSynthetizedValue isEmpty ifTrue: [ nil] ifFalse: [ aSynthetizedValue asArray first]
				]
		].

	aCachedSynthetizedValue := aRelatedValue isNil 
		ifTrue: [  self class alreadyInitializedSentinelValue]
		ifFalse: [  aRelatedValue].
	
	aSynthesisCache at: self put: aCachedSynthetizedValue.

	^aSynthetizedValue!

computeSynthetizedOverridenValueFrom: theObject 
	synthesisRelationship: 				theSynthesisRelationship 
	relatedTypeSynthesisRelationship: 	theRelatedTypeSynthesisRelationship 
	synthetizedValueInto: 					theSynthetizedValue 
	notToIncludeInto: 						theNotToIncludeSet

	| someBaseObjects aIsMultiplicityMany aStoredValue aNotToIncludeSet |

	theObject isNil ifTrue: [ ^nil].
	theRelatedTypeSynthesisRelationship isNil ifTrue: [ ^nil].
	theSynthetizedValue isNil ifTrue: [ ^nil].
	theNotToIncludeSet isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].
 	self isSynthetizedRelationship ifFalse: [ ^nil].
	self isPropagateSynthesisRelationship ifFalse: [ ^nil].	

	aIsMultiplicityMany := self isMultiplicityMany.
	aIsMultiplicityMany ifFalse: [ 
		^self computeSynthetizedOverridenSingleValueFrom: theObject 
			synthesisRelationship: theSynthesisRelationship 
			synthetizedValueInto: theSynthetizedValue 
	].

	aNotToIncludeSet := theNotToIncludeSet copy.
	
	aStoredValue := self getObjectRelationshipValueStored: theObject.

	someBaseObjects := theSynthesisRelationship getObjectRelationshipValue: theObject.
	(someBaseObjects isNil not and: [  someBaseObjects isEmpty not]) ifTrue: [ 

		aStoredValue isNil ifFalse: [
			aStoredValue do: [:aStoredRelatedObject |  | aRelatedObjectBaseObjects  |
				aRelatedObjectBaseObjects := theRelatedTypeSynthesisRelationship recurseCollectFrom: aStoredRelatedObject.
				aRelatedObjectBaseObjects isNil ifFalse: [ 
					aNotToIncludeSet addAll: aRelatedObjectBaseObjects
				]
			]
		].

		someBaseObjects do: [:aBaseObject |  | aBaseObjectSynthetizedRelationship |

			aBaseObjectSynthetizedRelationship := self  sameOrEquivalentRelationshipOn: aBaseObject.
			aBaseObjectSynthetizedRelationship isNil ifFalse: [ 
				aBaseObjectSynthetizedRelationship
					computeSynthetizedOverridenValueFrom: aBaseObject 
					synthesisRelationship: 					theSynthesisRelationship 
					relatedTypeSynthesisRelationship: 		theRelatedTypeSynthesisRelationship 
					synthetizedValueInto: 					theSynthetizedValue 
					notToIncludeInto: 						aNotToIncludeSet
			]
		]
	].

	aStoredValue isNil ifFalse: [
		aStoredValue do: [:aStoredRelatedObject |
			(theNotToIncludeSet includes: aStoredRelatedObject) ifFalse: [ 
				theSynthetizedValue add: aStoredRelatedObject
			]
		]
	]!

computeSynthetizedValueFrom: theObject
	| aSynthesisCache aSynthetizedValue aCachedSynthetizedValue aMetaInfo aSynthesisRelationship someBaseObjects aIsMultiplicityMany aStoredValue aRelatedTypeMetaInfo aRelatedTypeSynthesisRelationship |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].
 	self isSynthetizedRelationship ifFalse: [ ^nil].

	self isPropagateSynthesisRelationship ifTrue: [ 
		aRelatedTypeMetaInfo := self relatedType.
		aRelatedTypeMetaInfo isNil ifFalse: [ 
			aRelatedTypeSynthesisRelationship  := aRelatedTypeMetaInfo synthesisRelationship.
			aRelatedTypeSynthesisRelationship isNil ifFalse: [ 
				^self computeSynthetizedOverridenValueFrom: theObject
			]
		]
	].

	aIsMultiplicityMany := self isMultiplicityMany.

	aStoredValue := self getObjectRelationshipValueStored: theObject.

	aSynthetizedValue := aIsMultiplicityMany ifTrue: [ self isOrdered ifTrue: [ OrderedCollection new: 8] ifFalse: [ IdentitySet new: 13]] ifFalse: [ nil].


	(aIsMultiplicityMany or: [ aStoredValue isNil]) 
		ifTrue: [ 
			aMetaInfo := theObject metaInfo.
			aMetaInfo isNil ifTrue: [ ^nil].

			aSynthesisRelationship := aMetaInfo synthesisRelationship.
			aSynthesisRelationship isNil ifTrue: [ ^nil].
	
			someBaseObjects := aSynthesisRelationship getObjectRelationshipValue: theObject.
			someBaseObjects isNil ifFalse: [ 

				someBaseObjects do: [:aBaseObject |  

					| aBaseObjectMetaInfo aBaseObjectRelationship aBaseObjectRelationshipIsMultiplicityMany aBaseObjectRelatedObjects aBaseObjectRelatedObject |
					aBaseObjectMetaInfo := aBaseObject metaInfo.
					aBaseObjectRelationship := aBaseObjectMetaInfo == self type 
						ifTrue: [ self ]
						ifFalse: [ aBaseObjectMetaInfo effectiveFeatureNamed: self name].
					aBaseObjectRelationship isNil ifFalse: [ 
						aBaseObjectRelationshipIsMultiplicityMany := aBaseObjectRelationship isMultiplicityMany.
						aBaseObjectRelationshipIsMultiplicityMany == aIsMultiplicityMany ifTrue: [ 
							aBaseObjectRelationshipIsMultiplicityMany
								ifTrue: [ 
									aBaseObjectRelatedObjects := aBaseObjectRelationship getObjectRelationshipValue: aBaseObject.
									aSynthetizedValue addAll: aBaseObjectRelatedObjects
								]
								ifFalse: [ 
									aBaseObjectRelatedObject := aBaseObjectRelationship getObjectRelationshipValue: aBaseObject.
									aBaseObjectRelatedObject isNil ifFalse: [ 
										aSynthetizedValue := aBaseObjectRelatedObject
									]
								]
						]
					]	
				]
			].
			aIsMultiplicityMany ifTrue: [ aSynthetizedValue addAll: aStoredValue]
		]
		ifFalse: [ 
			aStoredValue isNil ifFalse: [ aSynthetizedValue := aStoredValue]
		].



	aSynthesisCache := theObject synthesisCache.
	aSynthesisCache isNil ifTrue: [ 
		aSynthesisCache := theObject initSynthesisCache
	].
	aCachedSynthetizedValue := aSynthetizedValue isNil 
		ifTrue: [  self class alreadyInitializedSentinelValue]
		ifFalse: [  aSynthetizedValue].
	
	aSynthesisCache at: self put: aCachedSynthetizedValue.

	^aSynthetizedValue!

invalidateSynthesisCacheObject: theObject
	| aSynthesisCache |

	theObject isNil ifTrue: [ ^self].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^self].
 
	self isSynthetizedRelationship ifFalse: [ ^self].

	aSynthesisCache := theObject synthesisCache.
	aSynthesisCache isNil ifTrue: [ ^self].

	aSynthesisCache removeKey: self ifAbsent: [ nil]!

isPropagateSynthesisRelationship
	^(self aspectNamed: 'PropagateSynthesis') isNil not!

isSynthesisAllBaseObjectsRelationship
	^(self aspectNamed: 'IsSynthesisAllBaseObjects') isNil not!

isSynthesisRelationship
	^(self aspectNamed: 'IsSynthesisBaseObjectsReference') isNil not!

isSynthetizedRelationship
	^(self aspectNamed: 'IsSynthetized') isNil not!

propagateSynthesisInvalidateObject: theObject
	| aMetaInfo aSynthesisRelationship aDerivedRelationship someDerivedObjects |

	theObject isNil ifTrue: [ ^self].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^self].
 
	self isSynthetizedRelationship ifFalse: [ ^self].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aSynthesisRelationship := aMetaInfo synthesisRelationship.
	aSynthesisRelationship isNil ifTrue: [ ^nil].

	aDerivedRelationship := aSynthesisRelationship inverse.
	aDerivedRelationship isNil ifTrue: [ ^nil].
	
	someDerivedObjects := aDerivedRelationship getObjectRelationshipValueTC: theObject.
	someDerivedObjects isNil ifFalse: [ 
		someDerivedObjects do: [:aDerivedObject |   | aDerivedObjectMetaInfo aDerivedObjectRelationship |
			aDerivedObjectMetaInfo := aDerivedObject metaInfo.
			aDerivedObjectRelationship := aDerivedObjectMetaInfo == self type 
				ifTrue: [ self ]
				ifFalse: [ aDerivedObjectMetaInfo effectiveFeatureNamed: self name].
			aDerivedObjectRelationship isNil ifFalse: [ 
				aDerivedObjectRelationship synthesisInvalidateObject: aDerivedObject
			]
		]
	]!

propagateSynthetizedInvalidateObject: theObject
	| aMetaInfo aSynthesisRelationship aDerivedRelationship someDerivedObjects |

	theObject isNil ifTrue: [ ^self].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^self].
 
	self isSynthetizedRelationship ifFalse: [ ^self].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aSynthesisRelationship := aMetaInfo synthesisRelationship.
	aSynthesisRelationship isNil ifTrue: [ ^nil].

	aDerivedRelationship := aSynthesisRelationship inverse.
	aDerivedRelationship isNil ifTrue: [ ^nil].
	
	someDerivedObjects := aDerivedRelationship getObjectRelationshipValueTC: theObject.
	someDerivedObjects isNil ifFalse: [ 
		someDerivedObjects do: [:aDerivedObject |   | aDerivedObjectMetaInfo aDerivedObjectRelationship |
			aDerivedObjectMetaInfo := aDerivedObject metaInfo.
			aDerivedObjectRelationship := aDerivedObjectMetaInfo == self type 
				ifTrue: [ self ]
				ifFalse: [ aDerivedObjectMetaInfo effectiveFeatureNamed: self name].
			aDerivedObjectRelationship isNil ifFalse: [ 
				aDerivedObjectRelationship synthetizedInvalidateObject: aDerivedObject
			]
		]
	]!

synthesisInvalidateObject: theObject

	| aMetaInfo someSynthetizedRelationships someInverseSynthetizedRelationships  |
	theObject isNil ifTrue: [ ^self].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^self].
 
	self isSynthesisRelationship ifFalse: [ ^self].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	someSynthetizedRelationships := aMetaInfo synthetizedRelationships.
	someSynthetizedRelationships isNil ifFalse: [ 
		someSynthetizedRelationships do: [:aSynthetizedRelationship |
			aSynthetizedRelationship invalidateSynthesisCacheObject: theObject.
		]
	].	

	someInverseSynthetizedRelationships := aMetaInfo inverseSynthetizedRelationships.
	someInverseSynthetizedRelationships isNil ifFalse: [ 
		someInverseSynthetizedRelationships do: [:aInverseSynthetizedRelationship |  | anInverse aRelatedValue |
			anInverse := aInverseSynthetizedRelationship inverse.
			(anInverse isNil not and: [ anInverse isPropagateSynthesisRelationship]) ifTrue:[
				aRelatedValue := aInverseSynthetizedRelationship getObjectRelationshipValueTC: theObject.
				aRelatedValue isNil ifFalse: [ 
					aInverseSynthetizedRelationship isMultiplicityMany
						ifTrue: [   
							aRelatedValue do: [:aRelatedObject | 
								anInverse synthetizedInvalidateObject: aRelatedObject
							]
						]
						ifFalse: [ 
							anInverse synthetizedInvalidateObject: aRelatedValue
						]
				]
			]
		]
	].	



	self propagateSynthesisInvalidateObject: theObject!

synthetizedInvalidateObject: theObject

	theObject isNil ifTrue: [ ^self].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^self].
 
	self isSynthetizedRelationship ifFalse: [ ^self].

	self invalidateSynthesisCacheObject: theObject.

	self propagateSynthetizedInvalidateObject: theObject!

synthetizedValueFrom: theObject
	| aSynthesisCache aSynthetizedValue |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [  ^theObject perform: self getSelector].
 	self isSynthetizedRelationship ifFalse: [ ^nil].

	aSynthesisCache := theObject synthesisCache.
	aSynthesisCache isNil ifFalse: [ 
		aSynthetizedValue := aSynthesisCache at: self ifAbsent: [ nil].
		aSynthetizedValue isNil ifFalse: [ 
			aSynthetizedValue == self class alreadyInitializedSentinelValue
				ifTrue: [ ^nil ]
				ifFalse: [ ^aSynthetizedValue]
		]
	].

	aSynthetizedValue := self computeSynthetizedValueFrom: theObject.
	^aSynthetizedValue! !

!CODERelationship publicMethodsFor: 'TRF-traversals'!

recurseCollectFrom: theObject 

	| aSet |
	theObject isNil ifTrue: [ ^self].
	aSet := IdentitySet new: 13.

	self recurseFrom: theObject collectInto: aSet orderedInto: nil.
	^aSet!

recurseCollectOrderedFrom: theObject 

	| aSet anOrdered |
	theObject isNil ifTrue: [ ^self].
	aSet := IdentitySet new: 13.
	anOrdered := OrderedCollection new: 8.

	self recurseFrom: theObject collectInto: aSet orderedInto: anOrdered.
	^anOrdered!

recurseFrom: theObject collectInto: theSet orderedInto: theOrdered

	| aRelatedValue aRelatedObjectRelationship |
	theObject isNil ifTrue: [ ^self].
	theSet isNil ifTrue: [ ^self].

	(theSet includes: theObject) ifTrue: [ ^self].

	theSet add: theObject.
	theOrdered isNil ifFalse: [ 
		theOrdered add: theObject
	].

	aRelatedValue := self getObjectRelationshipValueTC: theObject.
	aRelatedValue isNil ifTrue: [ ^self].

	self isMultiplicityMany
		ifTrue: [ 
			aRelatedValue do: [:aRelatedObject |   
				aRelatedObjectRelationship := self sameOrEquivalentRelationshipOn: aRelatedObject.
				aRelatedObjectRelationship isNil ifFalse: [ 
					aRelatedObjectRelationship recurseFrom: aRelatedObject collectInto: theSet orderedInto: theOrdered
				]
			]
		]
		ifFalse: [ 
			aRelatedObjectRelationship := self sameOrEquivalentRelationshipOn: aRelatedValue.
			aRelatedObjectRelationship isNil ifFalse: [ 
				aRelatedObjectRelationship recurseFrom: aRelatedValue collectInto: theSet orderedInto: theOrdered
			]
		]!

sameOrEquivalentRelationshipOn: theObject
	| aMetaInfo aRelationship |
	theObject isNil ifTrue: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	(aMetaInfo hasEffectiveFeature: self) ifTrue: [ ^self].

	aRelationship := aMetaInfo effectiveRelationshipNamed: self name.
	aRelationship isNil ifFalse: [ ^aRelationship].
	
	^self! !

!CODERelationship publicMethodsFor: 'TRF-type membership'!

objectInstanceHasFeature: theObject 

	| aMetaInfo aResult |

	theObject isNil ifTrue: [ ^false].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^false].

	aResult := aMetaInfo allEffectiveRelationships includes: self.
	^aResult!

objectInstanceHasFeature: theObject relatedIsTypeOfObjectInstance: theNewObject 

	| anInverse |
	theObject isNil ifTrue: [ ^false].
	theNewObject isNil ifTrue: [ ^false].

	((self objectInstanceHasFeature: theObject) and: [ 
		self relatedIsTypeOfObjectInstance: theNewObject]) ifFalse: [self halt.  ^false].
 
	anInverse := self inverse.
	anInverse isNil ifTrue: [ ^false].

	^((anInverse objectInstanceHasFeature: theNewObject) and: [ 
		anInverse relatedIsTypeOfObjectInstance: theObject])!

relatedIsTypeOfObjectInstance: theNewObject 

	| aResult aRelatedType |

	theNewObject isNil ifTrue: [ ^false].

	aRelatedType := self relatedType.
	aRelatedType isNil ifTrue: [ ^false].
	
	aResult := aRelatedType isTypeOfObjectInstance: theNewObject.
	^aResult! !

!CODEStructuralFeature publicMethodsFor: 'TRF-dependency'!

buildDependency

	self computationKind = self class computationKindAlways ifFalse: [ ^self].

	self buildDependencyWithExpression: self initializationExpression.!

buildDependencyWithCalcExpression: theExpression
	| someStrings someExpressionStrings somePathExpressions |

	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 2 ifTrue: [ ^nil].

	someStrings first asSymbol =  self class initExpressionCalc  ifFalse: [ ^nil].

	someExpressionStrings := someStrings copyFrom: 2 to: someStrings size.

	somePathExpressions := self class pathExpressionsFromCalcSubExpressions: someExpressionStrings.
	somePathExpressions do: [:aPathExpression |  
		self buildDependencyWithPath: aPathExpression
	].!

buildDependencyWithExpression: theExpression
	| someStrings aDerivationPath |

	theExpression isNil ifTrue: [ ^self].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^self].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 2 ifTrue: [ ^self].

	someStrings first asSymbol =  self class initExpressionNilValue  ifTrue: [  ^self ].

	someStrings first asSymbol =  self class initExpressionLiteralValue  ifTrue: [ ^self].
 
	someStrings first asSymbol =  self class initExpressionCalc  ifTrue: [
		^self buildDependencyWithCalcExpression: theExpression
	].

	aDerivationPath := someStrings copyFrom: 2 to: someStrings size.
	aDerivationPath isEmpty ifTrue: [ ^self].

	self buildDependencyWithPath: aDerivationPath!

buildDependencyWithPath: thePath
	| aType |

	thePath isNil ifTrue: [ ^self].
	(thePath isNil or: [ thePath isEmpty]) ifTrue: [ ^self].

	aType := self type.
	aType isNil ifTrue: [ ^self].

	aType buildDependencyForFeature: self withPath: thePath!

buildObserversAfter: thePreviousTypeObserver onDependency: theDependency path: theDerivationPath
	
	| anObserver aReferencedType aType |

	theDependency isNil ifTrue: [ ^self].
	theDerivationPath isNil ifTrue: [ ^self].
	
	aType := self type.
	aType isNil ifTrue: [ ^self].

	anObserver := self preferredTypeObserverClass after: thePreviousTypeObserver onDependency: theDependency forType: aType feature: self 
		path: theDerivationPath copy.
	anObserver isNil ifTrue: [ ^self].

	aReferencedType := self referencedType.
	aReferencedType isNil ifTrue: [ ^nil].

	theDerivationPath isEmpty ifTrue: [^self].

	(aReferencedType isPrimitive or: [ aReferencedType isEnumeration]) ifTrue: [ ^self "Path should have been empty upon reaching a primitive"].

	aReferencedType buildObserversAfter: anObserver onDependency: theDependency path: theDerivationPath!

buildObserversAfter: thePreviousTypeObserver onDependency: theDependency path: theDerivationPath forType: theType
	
	| anObserver aReferencedType someMemberInstanceTypes |

	theDependency isNil ifTrue: [ ^self].
	theType isNil ifTrue: [ ^self].
	theDerivationPath isNil ifTrue: [ ^self].
	
	anObserver := self preferredTypeObserverClass after: thePreviousTypeObserver onDependency: theDependency forType: theType feature: self 
		path: theDerivationPath copy.
	anObserver isNil ifTrue: [ ^self].

	aReferencedType := self referencedType.
	aReferencedType isNil ifTrue: [ ^nil].

	theDerivationPath isEmpty ifTrue: [^self].

	(aReferencedType isPrimitive or: [ aReferencedType isEnumeration]) ifTrue: [ ^self "Path should have been empty upon reaching a primitive"].

	someMemberInstanceTypes := aReferencedType memberInstanceTypes.
	(someMemberInstanceTypes isNil or: [ someMemberInstanceTypes isEmpty]) ifTrue: [ ^self].

	someMemberInstanceTypes do: [:aMemberType |
		aMemberType buildObserversAfter: anObserver onDependency: theDependency path: theDerivationPath
	]!

dependencies
	^dependencies!

dependenciesAdd: theDependency
	dependencies isNil ifTrue: [ dependencies := OrderedCollection new: 2].
	dependencies add: theDependency.
	self changed: #dependencies!

dependenciesRemove: theDependency
	dependencies isNil ifTrue: [ ^self].
	dependencies remove: theDependency ifAbsent: [ nil].
	self changed: #dependencies!

notifyChangeForObject: theObject value: theNewObject

	| aType |
	aType := self type.
	aType isNil ifTrue: [ ^self].

	(aType isTypeOfObjectInstance: theObject) ifFalse: [ ^self].

	self produceChangeEventForObject: theObject.

	self updateObject: theObject value: theNewObject.!

produceChangeEventForObject: theObject 

	theObject isNil ifTrue: [ ^self].

	CMTransaction  newTransactionDo: [
		CMGenericChangeEventsMaker from: theObject to: nil metaInfo: self
	].!

updateNaive: theChangedFeature object: theObject  value: theNewObject avoidReenter: theFeaturesToAvoidReeenter

	| aExpression someStrings aDerivationPath |
	theObject isNil ifTrue: [ ^self].
	theFeaturesToAvoidReeenter isNil ifTrue: [ ^self].
	(theFeaturesToAvoidReeenter includes: self) ifTrue: [ ^self].

	self computationKind = self class computationKindAlways ifFalse: [ ^self].

	aExpression := self initializationExpression.

	aExpression isNil ifTrue: [ ^self].
	(aExpression isNil or: [ aExpression isEmpty]) ifTrue: [ ^self].

	someStrings := aExpression asArrayOfSubstrings.
	someStrings  size < 2 ifTrue: [ ^self].

	someStrings first asSymbol =  self class initExpressionNilValue  ifTrue: [ ^self].

	someStrings first asSymbol =  self class initExpressionLiteralValue  ifTrue: [ ^self].
 
	aDerivationPath := someStrings copyFrom: 2 to: someStrings size.
	aDerivationPath isEmpty ifTrue: [ ^self].

	(aDerivationPath includes: theChangedFeature name) ifFalse: [ ^self].

	self produceChangeEventForObject: theObject!

updateNaive: theObject value: theNewObject

	| aType |
	aType := self type.
	aType isNil ifTrue: [ ^nil].

	aType updateNaive: self object: theObject value: theNewObject!

updateObject: theObject value: theNewObject

	| aType |

	aType := self type.
	aType isNil ifTrue: [ ^nil].

	aType update: self object: theObject value: theNewObject!

xbuildDependencyWithCalcExpression: theExpression
	| someStrings someSubExpressions aCurrentSubExpression someExpressionStrings |

	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 2 ifTrue: [ ^nil].

	someStrings first asSymbol =  self class initExpressionCalc  ifFalse: [ ^nil].



	someSubExpressions := OrderedCollection new: someStrings size.
	aCurrentSubExpression := OrderedCollection new: someStrings size.
	someExpressionStrings := someStrings copyFrom: 2 to: someStrings size.
	someExpressionStrings do: [:aString |
		(self class isExpressionOperator:  aString) 
			ifTrue: [ 
				aCurrentSubExpression isEmpty ifFalse: [ 
					someSubExpressions add: aCurrentSubExpression; add: aString.
					aCurrentSubExpression := OrderedCollection new: someStrings size.
				]
			]
			ifFalse: [ aCurrentSubExpression add: aString	]
	].
	aCurrentSubExpression isEmpty ifFalse: [ 
		someSubExpressions add:  aCurrentSubExpression
	].

	someSubExpressions  size < 2 ifTrue: [ ^nil].

	someSubExpressions do: [:aSubExpression |  
		((aSubExpression isKindOf: String) and: [self class isExpressionOperator: aSubExpression])  ifFalse: [ 
			(aSubExpression first first = $"  or: [  aSubExpression first first isDigit ]) ifFalse: [ 
				self buildDependencyWithPath: aSubExpression
			]
		]
	].! !

!CODEStructuralFeature publicMethodsFor: 'TRF-derivations'!

computeDerivedValueFrom: theObject expression: theExpression
	| someStrings aClone aDerivationPath aMetaInfo aValue aReferencedType aConstraint aConstrainedValue aStream aExpression |

	theObject isNil ifTrue: [ ^nil].
	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.

	someStrings first asSymbol =  self class initExpressionNilValue  ifTrue: [ 
		^nil
	].

	someStrings first asSymbol =  self class initExpressionLiteralValue  ifTrue: [
		aReferencedType := self referencedType.
		^aReferencedType isNil 
			ifTrue: [ nil]
			ifFalse: [ aReferencedType computeDerivedValueFromLiteralExpression: theExpression]
	].
 
	someStrings first asSymbol =  self class initExpressionRandomAndLiteralValue  ifTrue: [
		self getExpandRanliteralInitializationExpressionsParameterValue ifFalse: [ ^nil].
		aReferencedType := self referencedType.
		^aReferencedType isNil 
			ifTrue: [ nil]
			ifFalse: [ aReferencedType computeDerivedValueFromRandomAndLiteralExpression: theExpression]
	].

	someStrings  size < 2 ifTrue: [ ^nil].

	aDerivationPath := someStrings copyFrom: 2 to: someStrings size.
	aDerivationPath isEmpty ifTrue: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].


	((someStrings first asSymbol =  self class initExpressionCloneTrue or: [ 
		someStrings first asSymbol =  self class initExpressionCloneFalse]) and: [ 
		(someStrings at: 2) asSymbol =  self class initExpressionCalc
	]) ifTrue: [ 
		aStream := WriteStream on: (String new: someStrings size * 16).
		(someStrings copyFrom: 2 to: someStrings size) do: [:aString |
			aStream nextPutAll: aString; space
		].
		aExpression := aStream contents.
		^aMetaInfo computeDerivedValueFrom: theObject calcExpression: aExpression
	].

	someStrings first asSymbol =  self class initExpressionCalc  ifTrue: [
		^aMetaInfo computeDerivedValueFrom: theObject calcExpression: theExpression
	].

	someStrings first asSymbol =  self class initExpressionSmalltalk  ifTrue: [
		^aMetaInfo executeFrom: theObject smalltalkExpression: theExpression
	].

	aClone := someStrings first asSymbol =  self class initExpressionCloneTrue.

	aValue := aMetaInfo object: theObject derive: aDerivationPath clone: aClone.
	aValue isNil ifTrue: [ ^nil].

	aConstraint := self relatedConstraint.
	(aConstraint isNil or: [ aConstraint isEmpty]) ifTrue: [ ^aValue].
	aConstrainedValue := aValue select: [:aValueElement |   | aResult |
		aResult := (aValueElement metaInfo computeDerivedValueFrom: aValueElement  calcExpression: aConstraint).
		aResult isNil not and: [ aResult isEmpty not and: [ aResult first == true]]
	].


	^aConstrainedValue! !

!CODEStructuralFeature publicMethodsFor: 'TRF-initialization'!

initializeNewObjectComputedFeature: theNewObject
	
	| aNewObject aResult |

	theNewObject isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].

	aResult := self computeDerivedValueFrom: theNewObject expression: self initializationExpression.
	aNewObject := aResult isNil 
		ifTrue: [ nil]
		ifFalse: [ 
			self isMultiplicityMany
				ifTrue: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult asArray first]]
				ifFalse: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult asArray first]]
		].

	aNewObject isNil 
		ifTrue: [ self markNewObjectFeatureAsAlreadyInitialized: theNewObject]
		ifFalse: [ 
			self isMultiplicityMany
				ifFalse: [ 	self object: theNewObject setTC: aNewObject]
				ifTrue: [ self object: theNewObject addTC: aNewObject]
		]!

initializeNewObjectCreatedFeature: theNewObject
	
	| aNewObject aReferencedType |

	theNewObject isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].

	aReferencedType := self referencedType.
	aReferencedType isNil ifTrue: [ ^self].

	aNewObject := aReferencedType createObject.

	aNewObject isNil 
		ifTrue: [ self markNewObjectFeatureAsAlreadyInitialized: theNewObject]
		ifFalse: [ 
			self isMultiplicityMany
				ifFalse: [ 	self object: theNewObject setTC: aNewObject]
				ifTrue: [ self object: theNewObject addTC: aNewObject]
		]!

initializeNewObjectFeature: theNewObject
	
	theNewObject isNil ifTrue: [ ^self].

	self isExclussion ifTrue: [ ^self].
	self maxMult = self class maxMultNone ifTrue: [ ^self].

	self name = self class objectDomainCMGOAttributeName ifTrue: [ ^self].
	self name = self class requiredFeaturesParentAttributeName ifTrue:  [^self].
	self computationKind = self class computationKindNoComputation 
		ifTrue: [ 
			self minMult = self class minMultRequired ifTrue: [ 
				self initializeNewObjectCreatedFeature: theNewObject
			]
		]
		ifFalse: [ 
			(self computationKind = self class computationKindInitialValue or: [ 
				self computationKind = self class computationKindInitializedInConstructor  " and:[ 
				self valueType isPrimitive or: [ self valueType isEnumeration or: [ 
					self valueType nonVirtualType isPrimitive or: [ self valueType nonVirtualType isEnumeration]]]]"])
				ifTrue: [  self initializeNewObjectComputedFeature: theNewObject]
		]!

markNewObjectFeatureAsAlreadyInitialized: theNewObject
	theNewObject isNil ifTrue: [ ^self].

	CMGenericLinkMaker from: theNewObject to: self class alreadyInitializedSentinelValue metaInfo: self! !

!CODEStructuralFeature publicMethodsFor: 'TRF-initialization-connect'!

initializeConnectedObjectComputedFeature: theNewObject afterConnectionTo: theObject

	| aNewObject aResult |

	theNewObject isNil ifTrue: [ ^false].

	self isExclussion ifTrue: [ ^false].
	self maxMult = self class maxMultNone ifTrue: [ ^false].

	self computationKind = self class computationKindAfterConnection ifFalse: [ ^false].

	(self hasBeenAlreadyInitialized: theNewObject) ifTrue: [ ^false].

	aResult := self computeDerivedValueFrom: theNewObject expression: self initializationExpression.
	aNewObject := aResult isNil 
		ifTrue: [ nil]
		ifFalse: [ 
			self isMultiplicityMany
				ifTrue: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult asArray first]]
				ifFalse: [ aResult isEmpty ifTrue: [nil] ifFalse: [ aResult asArray first]]
		].
	
	aNewObject isNil ifTrue: [ ^false].

	self isMultiplicityMany
		ifFalse: [ 	self object: theNewObject setTC: aNewObject]
		ifTrue: [ self object: theNewObject addTC: aNewObject].

	^true! !

!CODEStructuralFeature publicMethodsFor: 'TRF-object accessing'!

hasBeenAlreadyInitialized: theObject
	| aValue |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^false].

	self computationKind = self class computationKindAlways ifTrue: [ ^false].

	aValue := theObject propertyAt: self name asSymbol.

	aValue = self class alreadyInitializedSentinelValue ifTrue: [ ^false].
	
	^aValue isNil not!

hasBeenAlreadyTriedToInitialize: theObject
	| aValue |

	theObject isNil ifTrue: [ ^nil].

	(theObject isKindOf: CMGenericObject)  ifFalse: [ ^false].

	self computationKind = self class computationKindAlways ifTrue: [ ^false].

	aValue := theObject propertyAt: self name asSymbol.

	aValue = self class alreadyInitializedSentinelValue ifTrue: [ ^true].
	
	^aValue isNil not!

isTypeOfObjectInstance: theObject

	| aType  aResult |

	theObject isNil ifTrue: [ ^false].
	
	aType := self type.
	aType isNil ifTrue: [ ^false].

	aResult := aType isTypeOfObjectInstance: theObject.
	^aResult! !

!CODEType class publicMethodsFor: 'preferences'!

preferredCMGenericObjectClass
	^self preferredPreferencesClass preferredCMGenericObjectClass! !

!CODEType publicMethodsFor: 'generic navigation'!

metaNameSelectorForObject: theObject

	| aNameAttribute |
	theObject isNil ifTrue: [ ^nil].
	
	aNameAttribute := self nameAttribute.
	aNameAttribute isNil ifTrue: [ ^nil].
	
	^aNameAttribute name asSymbol! !

!CODEType publicMethodsFor: 'preferences'!

preferredCMGenericObjectClass
	^self class preferredCMGenericObjectClass! !

!CODEType publicMethodsFor: 'TRF-change events'!

propagateChangeEventsFromObject: theObject feature: theFeature saved: theSaved

	| aFeatureValue |
	theObject isNil ifTrue: [ ^self].
	theFeature isNil ifTrue: [ ^self].

	(self hasOrInheritsFeature: theFeature) ifFalse: [ ^self].
	aFeatureValue := theFeature getObjectFeatureValue: theObject. 

	theObject notifyChangeEvent: theFeature name asSymbol withNew: aFeatureValue withSaved: theSaved!

propagateDeletionChangeEventsFromLinker: theLinker

	| anObject |
	theLinker isNil ifTrue: [ ^self].

	anObject := theLinker oneObject.
	anObject isNil ifTrue: [ ^self].

	anObject notifyDeletionChangeEvent! !

!CODEType publicMethodsFor: 'TRF-creation'!

createEnumerationObject

	| aNewObject anEnumValue someEnumAttributes aNonVirtualType |

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue:  [ ^nil].

	aNonVirtualType isEnumeration ifFalse: [ ^nil].

	someEnumAttributes := self allEnumerationAttributes.
	(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].

	anEnumValue := someEnumAttributes asArray first.

	anEnumValue isNil ifTrue: [ ^nil].
	anEnumValue isAttribute ifFalse: [ ^nil].
	anEnumValue isEnumerationValue ifFalse: [ ^nil].

	aNewObject := nil.

	^(CMTransaction  newTransactionDo: [  | |
		aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self enumValue: anEnumValue.
	]) 
		ifTrue: [aNewObject]
		ifFalse: [ nil].!

createEnumerationObjectFromLiteral: theLiteral

	| aNewObject someEnumAttributes anOwnEnumValue aNonVirtualType someStrings aLiteral |

	(theLiteral isNil or: [ theLiteral isEmpty]) ifTrue: [ ^nil].

	someStrings := theLiteral asArrayOfSubstrings.
	(someStrings isNil or: [ someStrings isEmpty]) ifTrue: [ ^nil].

	aLiteral := someStrings first.

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue:  [ ^nil].

	aNonVirtualType isEnumeration ifFalse: [ ^nil].

	someEnumAttributes := self allEnumerationAttributes.
	(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].
	
	anOwnEnumValue := someEnumAttributes detect: [:anEnumAttr | anEnumAttr name = aLiteral]
		ifNone: [ nil].
	anOwnEnumValue isNil ifTrue: [ ^nil].
	

	aNewObject := nil.

	^(CMTransaction  newTransactionDo: [  | |
		aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self enumValue: anOwnEnumValue.
	]) 
		ifTrue: [aNewObject]
		ifFalse: [ nil].!

createEnumerationObjectWithRequiredFeaturesObject: theRequiredFeaturesObject prefixes: thePrefixes

	| aNewObject anEnumValue someEnumAttributes aRequiredFeaturesMetaInfo aRequiredFeatureName aRequiredFeature aRequiredValue anOwnEnumValue aNonVirtualType |

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue:  [ ^nil].

	aNonVirtualType isEnumeration ifFalse: [ ^nil].

	(theRequiredFeaturesObject isNil or: [ 
		theRequiredFeaturesObject metaInfo attributes isEmpty or: [ thePrefixes isNil]]) ifTrue: [ 
		^self createEnumerationObject
	].

	aRequiredFeaturesMetaInfo := theRequiredFeaturesObject metaInfo.
	aRequiredFeaturesMetaInfo isNil ifTrue:  [ ^self createEnumerationObject].

	aRequiredFeatureName := self requiredFeatureNameFromPrefixes: thePrefixes.
	aRequiredFeature := aRequiredFeaturesMetaInfo featureOrInheritedNamed: aRequiredFeatureName.
	aRequiredFeature isNil ifTrue:  [ ^self createEnumerationObject].

	aRequiredValue := aRequiredFeature getObjectFeatureValueTC: theRequiredFeaturesObject.
	aRequiredValue isNil ifTrue: [ ^self createEnumerationObject].

	anEnumValue := aRequiredFeature isMultiplicityMany 
		ifFalse: [ aRequiredValue enumValue]
		ifTrue: [ 
			aRequiredValue isEmpty
				ifTrue: [ nil]
				ifFalse: [ aRequiredValue first enumValue]
		].
	anEnumValue isNil ifTrue: [ ^self createEnumerationObject].
	anEnumValue isAttribute ifFalse: [ ^self createEnumerationObject].
	anEnumValue isEnumerationValue ifFalse: [ ^self createEnumerationObject].

	someEnumAttributes := self allEnumerationAttributes.
	(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].
	
	anOwnEnumValue := someEnumAttributes detect: [:anEnumAttr | anEnumAttr name = anEnumValue name]
		ifNone: [ nil].
	anOwnEnumValue isNil ifTrue: [ ^self createEnumerationObject].
	

	aNewObject := nil.

	^(CMTransaction  newTransactionDo: [  | |
		aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self enumValue: anOwnEnumValue.
	]) 
		ifTrue: [aNewObject]
		ifFalse: [ nil].!

createEnumObject: theEnumValueName

	| aNewObject anEnumValue someEnumAttributes |

	someEnumAttributes := self allEnumerationAttributes.
	(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].

	anEnumValue := someEnumAttributes detect: [:anAttribute | anAttribute name = theEnumValueName] ifNone: [ nil].

	anEnumValue isNil ifTrue: [ ^nil].
	anEnumValue isAttribute ifFalse: [ ^nil].
	anEnumValue isEnumerationValue ifFalse: [ ^nil].

	aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self enumValue: anEnumValue.
	^aNewObject!

createNonPrimitiveObject

	| aNewObject  |

	aNewObject := nil.

	^(CMTransaction  newTransactionDo: [ 
		aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self.
		aNewObject isNil ifTrue: [  Transaction undoLastTransaction].
		self initializeNewObjectFeatures: aNewObject.
	]) 
		ifTrue: [aNewObject]
		ifFalse: [ nil].!

createNonPrimitiveObjectFromLiteral: theLiteral

	^nil!

createNonPrimitiveObjectWithRequiredFeaturesObject: theRequiredFeaturesObject

	| aNewObject  |

	aNewObject := nil.

	^(CMTransaction  newTransactionDo: [ 

		aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self.
		aNewObject isNil ifTrue: [  Transaction undoLastTransaction].
		self initializeNewObjectFeatures: aNewObject.
		self initializeNewObjectRequiredFeatures: aNewObject withRequiredFeaturesObject: theRequiredFeaturesObject.
	]) 
		ifTrue: [aNewObject]
		ifFalse: [ nil].!

createNonPrimitiveObjectWithRequiredFeaturesObject: theRequiredFeaturesObject prefixes: thePrefixes
	| aNewObject  |

	aNewObject := nil.

	^(CMTransaction  newTransactionDo: [ 

		aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self.
		aNewObject isNil ifTrue: [  Transaction undoLastTransaction].
		self initializeNewObjectFeatures: aNewObject.
		self initializeNewObjectRequiredFeatures: aNewObject 
			withRequiredFeaturesObject: theRequiredFeaturesObject
			prefixes: thePrefixes.
	]) 
		ifTrue: [aNewObject]
		ifFalse: [ nil].!

createObject

	| aNonVirtualType |
	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^nil].

	^aNonVirtualType isPrimitive 
		ifTrue: [ self createPrimitiveObject]
		ifFalse: [
			aNonVirtualType isEnumeration 
				ifTrue: [ self createEnumerationObject]
				ifFalse: [ self createNonPrimitiveObject]
		]!

createObjectFromLiteral: theLiteral

	| aNonVirtualType |
	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^nil].

	^aNonVirtualType isPrimitive 
		ifTrue: [ self createPrimitiveObjectFromLiteral: theLiteral]
		ifFalse: [
			aNonVirtualType isEnumeration 
				ifTrue: [ self createEnumerationObjectFromLiteral: theLiteral]
				ifFalse: [ self createNonPrimitiveObjectFromLiteral: theLiteral]
		]!

createObjectWithRequiredFeaturesObject: theRequiredFeaturesObject

	| aNonVirtualType |
	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^nil].

	^aNonVirtualType isPrimitive 
		ifTrue: [ self createPrimitiveObject]
		ifFalse: [
			aNonVirtualType isEnumeration 
				ifTrue: [ self createEnumerationObject]
				ifFalse: [ self createNonPrimitiveObjectWithRequiredFeaturesObject: theRequiredFeaturesObject]
		]!

createObjectWithRequiredFeaturesObject: theRequiredFeaturesObject prefixes: thePrefixes

	| aNonVirtualType |
	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^nil].

	^aNonVirtualType isPrimitive 
		ifTrue: [ 
			self createPrimitiveObjectWithRequiredFeaturesObject: theRequiredFeaturesObject prefixes: thePrefixes
		]
		ifFalse: [
			aNonVirtualType isEnumeration 
				ifTrue: [ 
					self createEnumerationObjectWithRequiredFeaturesObject: theRequiredFeaturesObject prefixes: thePrefixes
				]
				ifFalse: [ 
					self createNonPrimitiveObjectWithRequiredFeaturesObject: theRequiredFeaturesObject prefixes: thePrefixes
				]
		]!

createPrimitiveObject

	| aNonVirtualType anInitStr aNewObject |
	

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue:  [ ^nil].

	aNonVirtualType isPrimitive ifFalse: [ ^nil].

	anInitStr := self primitiveInitializationString.
	(anInitStr isNil not and: [ anInitStr isEmpty]) ifTrue: [ anInitStr := nil].

	aNewObject :=  self primitiveBroker create.
	^aNewObject!

createPrimitiveObjectFromLiteral: theLiteral

	| aNonVirtualType aNewObject |
	
	(theLiteral isNil or: [ theLiteral isEmpty]) ifTrue: [ ^nil].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue:  [ ^nil].

	aNonVirtualType isPrimitive ifFalse: [ ^nil].

	aNewObject := self primitiveBroker createFromString: theLiteral.

	^aNewObject!

createPrimitiveObjectWithRequiredFeaturesObject: theRequiredFeaturesObject prefixes: thePrefixes

	| aNonVirtualType aNewObject aRequiredFeaturesMetaInfo aRequiredFeatureName aRequiredFeature aRequiredValue aPrimitiveValue |
	
	(theRequiredFeaturesObject isNil or: [ 
		theRequiredFeaturesObject metaInfo attributes isEmpty or: [ thePrefixes isNil or: [ thePrefixes isEmpty]]]) ifTrue: [ 
		^self createPrimitiveObject
	].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue:  [ ^nil].

	aNonVirtualType isPrimitive ifFalse: [ ^nil].

	aRequiredFeaturesMetaInfo := theRequiredFeaturesObject metaInfo.
	aRequiredFeaturesMetaInfo isNil ifTrue:  [ ^self createPrimitiveObject].

	aRequiredFeatureName := self requiredFeatureNameFromPrefixes: thePrefixes.
	aRequiredFeature := aRequiredFeaturesMetaInfo featureOrInheritedNamed: aRequiredFeatureName.
	aRequiredFeature isNil ifTrue:  [ ^self createPrimitiveObject].

	aRequiredValue := aRequiredFeature getObjectFeatureValueTC: theRequiredFeaturesObject.
	aRequiredValue isNil ifTrue: [ ^self createPrimitiveObject].

	aPrimitiveValue := aRequiredFeature isMultiplicityMany 
		ifFalse: [ aRequiredValue]
		ifTrue: [ 
			aRequiredValue isEmpty
				ifTrue: [ nil]
				ifFalse: [ aRequiredValue first ]
		].

	aNewObject := self primitiveBroker createFromObject: aPrimitiveValue.

	^aNewObject!

createRequiredFeaturesObjectForParent: theObject fromFeatureMetaInfo: theFeatureMetaInfo

	| aNonVirtualType aNewObject aRequiredFeaturesParentAttributeName aRequiredFeaturesParentAttribute someEffectiveAttributes someAttributesAndExtents  anAggregatedIntoAttribute anInverseFeatureMetaInfo someAttributes |

	theObject isNil ifTrue: [ ^nil].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^nil].

	(self isPrimitive or: [ self isEnumeration or: [ 
		aNonVirtualType isPrimitive  or: [ aNonVirtualType isEnumeration]]]) ifTrue: [  ^nil].

	aNewObject := nil.

	aRequiredFeaturesParentAttributeName := self class requiredFeaturesParentAttributeName.
	aRequiredFeaturesParentAttribute := self effectiveFeatureNamed: aRequiredFeaturesParentAttributeName.

  	someEffectiveAttributes := self allEffectiveAttributes.
	someAttributesAndExtents := OrderedCollection new: someEffectiveAttributes size.

	someEffectiveAttributes do: [:anAttribute | | anExpression aTypeMetaInfo someExtentObjects |
		anExpression := anAttribute initializationExpression.
		(anExpression isNil not and: [ anExpression  asSymbol  = self class initExpressionExtent ]) ifTrue: [ 
			aTypeMetaInfo := anAttribute referencedType.
			aTypeMetaInfo isNil ifFalse: [ 
				someExtentObjects := aTypeMetaInfo extentFromObject: theObject includeSubTypes: true.
				(someExtentObjects isNil not and: [ someExtentObjects isEmpty not]) ifTrue: [ 
					someAttributesAndExtents add: (Array with: anAttribute with: someExtentObjects)
				]
			]
		]
	].

	anAggregatedIntoAttribute := nil.
	theFeatureMetaInfo isRelationship ifTrue: [ 
		anInverseFeatureMetaInfo := theFeatureMetaInfo inverse.
		someAttributes := self allEffectiveAttributes.
		anAggregatedIntoAttribute := someAttributes detect: [:anAttribute |  
			anAttribute name = anInverseFeatureMetaInfo name ] ifNone: [ nil]
	].

	^(CMTransaction  newTransactionDo: [ 
		aNewObject := self preferredCMGenericObjectClass newWithMetaInfo: self.
		aNewObject isNil ifTrue: [  Transaction undoLastTransaction].
		aRequiredFeaturesParentAttribute isNil ifFalse: [ 
			aRequiredFeaturesParentAttribute object: aNewObject setTC: theObject.
		].
		someAttributesAndExtents do: [:anAttributeAndExtent |  | anAttribute anExtent |
			anAttribute := anAttributeAndExtent first.
			anExtent := anAttributeAndExtent at: 2.
			anExtent do: [:anExtentObject | 
				anAttribute object: aNewObject addTC: anExtentObject
			]
		].
		anAggregatedIntoAttribute isNil ifFalse: [ 
			anAggregatedIntoAttribute object: aNewObject setTC: theObject.
		].
		self initializeNewObjectFeatures: aNewObject.

	]) 
		ifTrue: [aNewObject]
		ifFalse: [ nil].! !

!CODEType publicMethodsFor: 'TRF-deletion'!

deleteObject: theObject

	| someFeatures |
	(self isTypeOfObjectInstance: theObject) ifFalse:  [ ^nil].

	someFeatures := self allEffectiveStructuralFeatures.
	^self preferredTransactionClass newTransactionDo: [
		CMGenericDeletionChangeEventsMaker delete: theObject metaInfo: self.
		someFeatures do: [:aFeature |
			aFeature disconnectForDelete: theObject
		].
		
	]! !

!CODEType publicMethodsFor: 'TRF-dependency'!

attachObserver: theObserver

	observers isNil ifTrue: [ observers := OrderedCollection new: 2].
	(observers includes: theObserver) ifTrue: [ ^self].

	observers add: theObserver.

	self changed: #observers!

buildDependencies

	| aNonVirtualType someEffectiveFeatures |
	(self isPrimitive or: [ self isEnumeration]) ifTrue: [ ^self ].
	
	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^self ].
	(aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]) ifTrue: [ ^self ].

	someEffectiveFeatures := self allLocalEffectiveFeatures.
	(someEffectiveFeatures isNil or: [ someEffectiveFeatures isEmpty]) ifTrue: [ ^self].

	someEffectiveFeatures do: [:aFeature | aFeature buildDependency]!

buildDependencyForFeature: theFeature withPath: theDerivationPath
	
	| aDependency |

	theFeature isNil ifTrue: [ ^self].
	(theDerivationPath isNil or: [ theDerivationPath isEmpty  ]) ifTrue: [ ^self].

	(self isPrimitive or: [ self isEnumeration]) ifTrue: [ ^self].

	aDependency := self preferredTypeDependencyClass forType: self feature: theFeature path: theDerivationPath.
	aDependency isNil ifTrue: [ ^self].
	theFeature dependenciesAdd: aDependency.

	self buildObserversAfter: nil onDependency: aDependency path: theDerivationPath!

buildObserversAfter: thePreviousTypeObserver onDependency: theDependency path: theDerivationPath
	
	| aFeatureName aFeature aDerivationPath |

	theDependency isNil ifTrue: [ ^self].
	theDerivationPath isNil ifTrue: [ ^self].
	
	theDerivationPath isEmpty ifTrue: [^self].

	(self isPrimitive or: [ self isEnumeration]) ifTrue: [ ^self "The Derivation path should have been empty upon reaching a primitive"].

	aDerivationPath := theDerivationPath size < 2 
		ifTrue: [  Array new]
		ifFalse: [ theDerivationPath copyFrom: 2 to: theDerivationPath size].

"	(observers isNil not and: [ observers isEmpty not]) ifTrue:  [ 
		anIndex := thePreviousTypeObserver isNil ifTrue: [ 1] ifFalse: [ thePreviousTypeObserver index + 1].
		(observers detect: [:anObserver |
			anObserver dependency == theDependency and: [ anObserver index = anIndex and: [ anObserver path = aDerivationPath]]
		] ifNone: [ nil]) isNil ifFalse: [ ^self].
	].
"
	aFeatureName := theDerivationPath first.
	aFeature := self effectiveFeatureNamed: aFeatureName.
	aFeature isNil ifTrue: [ ^self].

	aFeature buildObserversAfter: thePreviousTypeObserver onDependency: theDependency path: aDerivationPath!

detachObserver: theObserver

	(observers isNil or: [ observers isEmpty]) ifTrue: [ ^self].

	observers remove: theObserver ifAbsent: [ nil].

	self changed: #observers!

update: theChangedFeature object: theObject value:  theNewObject 

	| aChangedFeature someRefinedFeatures |
	theChangedFeature isNil ifTrue: [ ^self].
"Transcript show: 'Updating '; show: theChangedFeature name ; show: ' type '; show: self name; 
	show: ' objectNamed '; show: (theObject metaInfo getObjectNameValue: theObject); cr."

	self updateNoSuperTypes: theChangedFeature object: theObject value:  theNewObject.

	(self allSuperTypesWithFeature: theChangedFeature) do: [:aSuperType |
		aSuperType updateNoSuperTypes: theChangedFeature object: theObject value:  theNewObject 
	].

	theChangedFeature isRefinement ifTrue: [ 
		aChangedFeature := theChangedFeature.
		[aChangedFeature isNil ] whileFalse: [ 
			someRefinedFeatures := aChangedFeature allRefinedFeatures.
			someRefinedFeatures isEmpty
				ifTrue: [ aChangedFeature := nil]
				ifFalse: [ 
					someRefinedFeatures remove: aChangedFeature.
					someRefinedFeatures isEmpty
						ifTrue: [ aChangedFeature := nil]
						ifFalse: [ 
							aChangedFeature := someRefinedFeatures asArray first.
							aChangedFeature type updateNoSuperTypes: aChangedFeature object: theObject value:  theNewObject.
							(aChangedFeature type allSuperTypesWithFeature: aChangedFeature) do: [:aSuperType |
								aSuperType updateNoSuperTypes: aChangedFeature object: theObject value:  theNewObject 
							].
							aChangedFeature isRefinement ifFalse: [ aChangedFeature := nil]
						]
				]
		]
	].!

updateFromDependency: theDependency object: theObject value: theNewObject	

	| |

	theDependency isNil ifTrue: [ ^self].
	theObject isNil ifTrue: [ ^self].
	
	(self isPrimitive or: [ self isEnumeration]) ifTrue: [ ^self ].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^self].

"
	aDependencyFeature := theDependency feature.
	aDependencyFeature isNil ifTrue: [ ^self].

	aFeatureName := aDependencyFeature name.

	aFeature := self effectiveFeatureNamed: aFeatureName.
	aFeature isNil ifTrue: [ ^self].

	aDerivationPath := theDerivationPath size < 2 
		ifTrue: [  Array new]
		ifFalse: [ theDerivationPath copyFrom: 2 to: theDerivationPath size].

	aFeature buildObserversOnDependency: theDependency path: aDerivationPath forType: self"!

updateNaive: theChangedFeature object: theObject value:  theNewObject 

	| someFeatures someFeaturesToAvoidReeenter |
	theChangedFeature isNil ifTrue: [ ^self].

	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^self].

	someFeaturesToAvoidReeenter := IdentitySet new: 13.
	someFeaturesToAvoidReeenter add: theChangedFeature.
	someFeatures do: [:aFeature |
		aFeature updateNaive: theChangedFeature object: theObject value:  theNewObject 
			avoidReenter: someFeaturesToAvoidReeenter
	]!

updateNoSuperTypes: theChangedFeature object: theObject value:  theNewObject 

	theChangedFeature isNil ifTrue: [ ^self].

	(self hasEffectiveFeature: theChangedFeature) ifFalse: [ ^self].

	(observers isNil or: [ observers isEmpty]) ifFalse: [ 
		observers do: [:anObserver |
			anObserver update: theChangedFeature object: theObject value: theNewObject 
		]
	].! !

!CODEType publicMethodsFor: 'TRF-derivations'!

canEvaluateOnInstances: theExpression
	| someStrings aDerivationPath |

	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^true].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 2 ifTrue: [ ^false].

	aDerivationPath := someStrings copyFrom: 2 to: someStrings size.
	aDerivationPath isEmpty ifTrue: [ ^false].

	^self canEvaluatePathOnInstances: aDerivationPath!

canEvaluatePathOnInstances: theDerivationPath 
	
	| aFeatureName aFeature aDerivationPath |
	theDerivationPath isNil ifTrue: [ ^true].
	
	theDerivationPath isEmpty ifTrue: [ ^true].

	(self isPrimitive or: [ self isEnumeration]) ifTrue: [ ^false].

	aFeatureName := theDerivationPath first.
	aFeature := self effectiveFeatureNamed: aFeatureName.
	aFeature isNil ifTrue: [ ^false].

	aDerivationPath := theDerivationPath size < 2 
		ifTrue: [  Array new]
		ifFalse: [ theDerivationPath copyFrom: 2 to: theDerivationPath size].

	^aFeature canEvaluatePathOnInstances: aDerivationPath!

computeDerivedValueFrom: theObject calcExpression: theExpression
	| someStrings someSubExpressions someExpressionStrings aClone aFirstIndex aCloneDirective |

	theObject isNil ifTrue: [ ^nil].
	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 2 ifTrue: [ ^nil].

	someStrings first asSymbol =  self class initExpressionCalc  ifFalse: [ ^nil].

	aClone := false.
	aFirstIndex := 2.
	aCloneDirective := (someStrings at: 2) asSymbol.
	(aCloneDirective =  self class initExpressionCloneTrue  or: [ 
		aCloneDirective =  self class initExpressionCloneFalse]) ifTrue: [ 
		someStrings  size < 3 ifTrue: [ ^nil].
		aClone := (someStrings at: 2) =  self class initExpressionCloneTrue.
		aFirstIndex := 3.
	].

	someExpressionStrings := someStrings copyFrom: aFirstIndex to: someStrings size.
	someSubExpressions := self class calcSubExpressions: someExpressionStrings.
	someSubExpressions  size < 2 ifTrue: [ ^nil].
 
	^self computeDerivedValueFrom: theObject calcSubExpressions: someSubExpressions clone: aClone!

computeDerivedValueFrom: theObject calcSubExpressions: theSubExpressions clone: theClone
	| aPrevValue aCurrentOperator  aPrevExpression anEndQuotePosition  someNestedSubExpressions |

	theObject isNil ifTrue: [ ^nil].
	(theSubExpressions isNil or: [ theSubExpressions isEmpty]) ifTrue: [ ^nil].
 
	aPrevValue := nil.
	aCurrentOperator := nil.
	aPrevExpression := nil.
	theSubExpressions do: [:aSubExpression |   | aValue  |
		((aSubExpression isKindOf: String) and: [self class isExpressionOperator: aSubExpression]) 
			ifTrue: [ 
				(self class isAlgorithmExpressionOperator: aSubExpression)
					ifTrue: [ 
						aPrevExpression isNil ifFalse: [ 
							aSubExpression asSymbol = self class initExpressionAlgorithmOperatorRecurse 
								ifTrue: [  self halt: 'Please, verify that the "recurse" algorithm works'.
									aPrevValue := self class recurse: aPrevValue expression: aPrevExpression
								]
								ifFalse: [ 
									aSubExpression asSymbol = self class initExpressionAlgorithmOperatorRecurseCollect 
										ifTrue: [ 
											aPrevValue := self class recurseCollect: aPrevValue expression: aPrevExpression
										]
										ifFalse: [ 

										]
								]
						]
					]
					ifFalse: [ 
						(self class isUnaryExpressionOperator: aSubExpression) 
							ifTrue: [  aPrevValue := self class compute: aPrevValue operator: aSubExpression ]
							ifFalse: [  aCurrentOperator := aSubExpression]
					].
			]
			ifFalse: [ 		
				aSubExpression first first = $" 
					ifTrue: [ 
						anEndQuotePosition := aSubExpression first findString: '"' startingAt: 2.
						aValue := aSubExpression first copyFrom: 2 to: 
							(anEndQuotePosition = 0 ifTrue: [ aSubExpression first size] ifFalse: [ anEndQuotePosition - 1]).
					]
					ifFalse: [ 
						(aSubExpression first asSymbol = self class initExpressionStartSubExpression and: [ aSubExpression last  asSymbol = self class initExpressionEndSubExpression])
							ifTrue: [ 
								aValue := aSubExpression size < 3 
									ifTrue: [ nil]
									ifFalse: [ 
										someNestedSubExpressions := aSubExpression copyFrom: 2 to: aSubExpression size - 1.
										self computeDerivedValueFrom: theObject calcSubExpressions: someNestedSubExpressions clone: theClone
									]
							]
							ifFalse: [ 


								aValue := aSubExpression first first isDigit 
									ifTrue:  [
										Object errorSignal 
											handle: [:anEx | anEx returnWith: 0]
											do: [ Number readFrom: aSubExpression first readStream]
									]
									ifFalse: [ 
										self object: theObject derive: aSubExpression 
											clone: ((aSubExpression == theSubExpressions last) and: [ theClone])
									]
							]
					].
				
				"(aValue isNil not and: [ (aValue isKindOf: Collection) and: [ (aValue isKindOf: String) not]]) ifTrue: [ 
					aValue := aValue asArray first
				]."
				aCurrentOperator isNil 
					ifTrue:  [  aPrevValue := aValue]
					ifFalse: [    
						aPrevValue := self class compute: aPrevValue operator: aCurrentOperator value: aValue.
					].
				aPrevExpression := aSubExpression
			]
	].

	(aPrevValue isNil not and: [ (aPrevValue isKindOf: CMGenericObject) not and: [ 
		(aPrevValue isKindOf: String) not and: [ aPrevValue isKindOf: Collection]]]) ifTrue: [ 
		^aPrevValue
	].

	^Array with: aPrevValue!

computeDerivedValueFrom: theObject expression: theExpression
	| someStrings aClone aDerivationPath aValue aMetaInfo |


	theObject isNil ifTrue: [ ^nil].
	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.

	someStrings first asSymbol =  self class initExpressionNilValue  ifTrue: [ 
		^nil
	].

	someStrings first asSymbol =  self class initExpressionLiteralValue  ifTrue: [
		^self computeDerivedValueFromLiteralExpression: theExpression
	].
 
	someStrings first asSymbol =  self class initExpressionRandomAndLiteralValue  ifTrue: [
		^self computeDerivedValueFromRandomAndLiteralExpression: theExpression
	].

	someStrings  size < 2 ifTrue: [ ^nil].

	someStrings first asSymbol  =  self class initExpressionCalc  ifTrue: [
		^self computeDerivedValueFrom: theObject calcExpression: theExpression
	].

	someStrings first asSymbol =  self class initExpressionSmalltalk  ifTrue: [
		^self executeFrom: theObject smalltalkExpression: theExpression
	].

	aClone := someStrings first =  self class initExpressionCloneTrue.

	aDerivationPath := someStrings copyFrom: 2 to: someStrings size.
	aDerivationPath isEmpty ifTrue: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo object: theObject derive: aDerivationPath clone: aClone.
	^aValue!

computeDerivedValueFromLiteralExpression: theExpression
	| someStrings aStream aLiteral aDerivedValue |

	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  isEmpty ifTrue: [ ^nil].

	(someStrings first asSymbol =  self class initExpressionLiteralValue or: [ 
		someStrings first asSymbol =  self class initExpressionRandomAndLiteralValue]) ifFalse: [ ^nil].

	aLiteral := someStrings size > 1
		ifFalse: [ '']
		ifTrue:[ 	
			someStrings size = 2 
				ifTrue: [ someStrings at: 2] 
				ifFalse: [ 
					aStream := WriteStream on: (String new: 32 * (someStrings size - 1)).
					2 to: someStrings size do: [:anIndex | 
						anIndex > 2 ifTrue: [ aStream space].
						aStream nextPutAll: (someStrings at: anIndex)
					].
					aStream contents
				]
		].

	aDerivedValue :=  self createObjectFromLiteral: aLiteral.

	^aDerivedValue isNil ifTrue: [ nil] ifFalse: [ Array with: aDerivedValue]!

computeDerivedValueFromRandomAndLiteralExpression: theExpression
	| someStrings aLiteralExpression aRandomString aNewExpression aStream |

	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 1 ifTrue: [ ^nil].

	someStrings first asSymbol =  self class initExpressionRandomAndLiteralValue  ifFalse: [ ^nil].

	aRandomString := (Random new next * 1000) floor asString.

	aNewExpression := self class initExpressionRandomAndLiteralValue , ' ', aRandomString , ' ', (someStrings size > 1
		ifFalse: [ '']
		ifTrue:[ 	
			someStrings size = 2 
				ifTrue: [ someStrings at: 2] 
				ifFalse: [ 
					aStream := WriteStream on: (String new: 32 * (someStrings size - 1)).
					2 to: someStrings size do: [:anIndex | 
						anIndex > 2 ifTrue: [ aStream space].
						aStream nextPutAll: (someStrings at: anIndex)
					].
					aStream contents
				]
		]).

	aLiteralExpression := self computeDerivedValueFromLiteralExpression: aNewExpression.
	^aLiteralExpression!

executeFrom: theObject smalltalkExpression: theExpression
	| someStrings aReturn aReceiverClassName aMethodSelector someArguments aClass |

	theObject isNil ifTrue: [ ^nil].
	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 3 ifTrue: [ ^nil].

	someStrings first asSymbol =  self class initExpressionSmalltalk  ifFalse: [ ^nil].

	aReceiverClassName := someStrings at: 2.
	aReceiverClassName isEmpty ifTrue: [ ^nil].
	aReceiverClassName := aReceiverClassName asSymbol.

	aMethodSelector := someStrings at: 3.
	aMethodSelector isEmpty ifTrue: [ ^nil].
	aMethodSelector := aMethodSelector asSymbol.

	someArguments := someStrings size > 3
		ifFalse: [ nil]
		ifTrue: [ 
			(someStrings copyFrom: 4 to: someStrings size) collect: [:aString |
				aString first = $# ifTrue: [ aString asSymbol] ifFalse: [
				aString first isDigit ifTrue: [ Number readFrom: aString readStream] ifFalse: [ 
				aString asSymbol = self class initExpressionSmalltalkArgument ifTrue: [  theObject] ifFalse: [ 
				aString
			]]]]
		].

	aClass := Smalltalk at: aReceiverClassName asSymbol ifAbsent: [ nil].
	aClass isNil ifTrue: [ ^nil].

	
	aReturn := Object errorSignal 
		handle: [:anEx | self halt. anEx returnWith: nil]
		do: [ 
				(someArguments isNil or: [ someArguments isEmpty ])
					ifTrue: [ aClass perform: aMethodSelector] 
					ifFalse: [ aClass perform: aMethodSelector withArguments: someArguments]
		].

	^Array with: aReturn!

extentFromObject: theObject includeSubTypes: theIncludeSubTypes

	| aRootObject someExtentObjects |

	theObject isNil ifTrue: [ ^nil].

	aRootObject := self rootFromObject: theObject.
	aRootObject isNil ifTrue: [ ^nil].	

	someExtentObjects := self scanAggregations: aRootObject forObjectsOfTypes: (theIncludeSubTypes ifTrue: [ self withAllSubtypes] ifFalse: [ Array with: self]).
	^someExtentObjects!

object: theObject derive: theDerivationPath clone: theClone
	
	| aFeatureName aFeature aDerivationPath |
	theObject isNil ifTrue: [ ^nil].
	theDerivationPath isNil ifTrue: [ ^nil].
	
	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	theDerivationPath isEmpty ifTrue: [ 
		^Array with: (theClone == true
			ifFalse: [ theObject] 
			ifTrue: [ self cloneObject: theObject])
	].

	(self isPrimitive or: [ self isEnumeration]) ifTrue: [ ^nil "The Derivation path should have been empty upon reaching a primitive"].

	aFeatureName := theDerivationPath first.
	aFeature := self effectiveFeatureNamed: aFeatureName.
	aFeature isNil ifTrue: [ ^nil].

	aDerivationPath := theDerivationPath size < 2 
		ifTrue: [  Array new]
		ifFalse: [ theDerivationPath copyFrom: 2 to: theDerivationPath size].

	^aFeature object: theObject derive: aDerivationPath clone: theClone!

rootFromObject: theObject 

	| aMetaInfo someRelationships |
	theObject isNil ifTrue: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^theObject].

	someRelationships := aMetaInfo allEffectiveAggregatedIntoRelationships.
	(someRelationships isNil or: [ someRelationships isEmpty]) ifTrue: [ ^theObject].

	someRelationships do: [:aRelationship |  | anAggregatedIntoObject |
		anAggregatedIntoObject :=  aRelationship getObjectFeatureValueTC:  theObject.
		anAggregatedIntoObject isNil ifFalse: [ ^self rootFromObject: anAggregatedIntoObject]
	].

	^theObject!

scanAggregations: theObject forObjectsOfTypes:  theTypes

	| aExtent anAlreadyScanned |
	theObject isNil ifTrue: [ ^nil].

	aExtent := IdentitySet new: 113.
	anAlreadyScanned := IdentitySet new: 1111.

	self scanAggregations: theObject forObjectsOfTypes: theTypes on: aExtent alreadyScanned: anAlreadyScanned.

	^aExtent!

scanAggregations: theObject forObjectsOfTypes: theTypes on: theExtent alreadyScanned: theAlreadyScanned

	| aMetaInfo someRelationships  |
	theObject isNil ifTrue: [ ^self].
	theExtent isNil ifTrue: [ ^self].
	theAlreadyScanned isNil ifTrue: [ ^self].

	(theAlreadyScanned includes: theObject) ifTrue: [ ^self].
	theAlreadyScanned add: theObject.

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	(theTypes includes: aMetaInfo) ifTrue: [ theExtent add: theObject].

	someRelationships := aMetaInfo allEffectiveAggregationRelationships.
	(someRelationships isNil or: [ someRelationships isEmpty]) ifTrue: [ ^self].

	someRelationships do: [:aRelationship |  | anAggregatedValue |
		anAggregatedValue :=  aRelationship getObjectFeatureValueTC:  theObject.
		anAggregatedValue isNil ifFalse: [ 
			aRelationship isMultiplicityMany
				ifFalse: [ self scanAggregations: anAggregatedValue forObjectsOfTypes: theTypes on: theExtent alreadyScanned: theAlreadyScanned]
				ifTrue: [ 
					anAggregatedValue do: [:anAggregatedObject |
						 self scanAggregations: anAggregatedObject forObjectsOfTypes: theTypes on: theExtent alreadyScanned: theAlreadyScanned
					]
				]
		]
	].!

xcalcSubExpressions: theExpression
	| someStrings someSubExpressions aCurrentSubExpression someExpressionStrings aFirstIndex aCloneDirective |

	theExpression isNil ifTrue: [ ^nil].
	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].

	someStrings := theExpression asArrayOfSubstrings.
	someStrings  size < 2 ifTrue: [ ^nil].

	someStrings first asSymbol =  self class initExpressionCalc  ifFalse: [ ^nil].

	aFirstIndex := 2.
	aCloneDirective := (someStrings at: 2) asSymbol.
	(aCloneDirective =  self class initExpressionCloneTrue  or: [ 
		aCloneDirective =  self class initExpressionCloneFalse]) ifTrue: [ 
		someStrings  size < 3 ifTrue: [ ^nil].
		aFirstIndex := 3.
	].

	someSubExpressions := OrderedCollection new: someStrings size.
	aCurrentSubExpression := OrderedCollection new: someStrings size.
	someExpressionStrings := someStrings copyFrom: aFirstIndex to: someStrings size.
	someExpressionStrings do: [:aString |
		(self class isExpressionOperator:  aString) 
			ifTrue: [ 
				aCurrentSubExpression isEmpty ifFalse: [ 
					someSubExpressions add: aCurrentSubExpression; add: aString.
					aCurrentSubExpression := OrderedCollection new: someStrings size.
				]
			]
			ifFalse: [ aCurrentSubExpression add: aString	]
	].
	aCurrentSubExpression isEmpty ifFalse: [ 
		someSubExpressions add:  aCurrentSubExpression
	].

	someSubExpressions  size < 2 ifTrue: [ ^nil].
 
	^someSubExpressions! !

!CODEType publicMethodsFor: 'TRF-domain'!

anchorNewObject: theNewObject inDomain: theDomainCMGO

	| aDomainMetaInfo aDomainModel aTypeDomain aHomeCMGO aHomeMetaInfo aAttributeHomeRoots aNewObjectMetaInfo aAttributeObjectDomain someSuperTypes |

	theNewObject isNil ifTrue: [ ^self].
	theDomainCMGO isNil ifTrue: [ ^self].

	aNewObjectMetaInfo := theNewObject metaInfo.
	aNewObjectMetaInfo isNil ifTrue: [ ^self].

	(self isTypeOfObjectInstance: theNewObject) ifFalse: [ ^self].

	(self isVirtual or: [ self isHomeRooted not]) ifTrue: [ 
		someSuperTypes := self superTypes.
		(someSuperTypes isNil or: [ someSuperTypes isEmpty]) ifFalse: [ 
			someSuperTypes do: [:aSuperType |
				aSuperType anchorNewObject: theNewObject inDomain: theDomainCMGO
			]
		].
		^self
	].


	aDomainMetaInfo := theDomainCMGO metaInfo.
	aDomainMetaInfo isNil ifTrue: [ ^self].

	aDomainModel := aDomainMetaInfo model.
	aDomainModel isNil ifTrue: [ ^self].

	aTypeDomain := aDomainModel resolveReferencedTypeName: self class domainCMGOTypeName 
		moduleNames: self class domainCMGOModuleNames.
	aTypeDomain isNil ifTrue: [ ^self].

	aDomainMetaInfo == aTypeDomain ifFalse: [ ^self].
	
	aHomeCMGO :=  self findHomeInDomain: theDomainCMGO.
	aHomeCMGO isNil ifTrue: [ ^self].

	aHomeMetaInfo := aHomeCMGO metaInfo.
	aHomeMetaInfo isNil ifTrue: [ ^self].

	aAttributeHomeRoots := aHomeMetaInfo attributeOrInheritedNamed:  self class homeRootsCMGODomainAttributeName.
	aAttributeHomeRoots isNil ifTrue: [ ^self].

	aAttributeHomeRoots object: aHomeCMGO addTC: theNewObject.

	aAttributeObjectDomain := aNewObjectMetaInfo attributeOrInheritedNamed:  self class objectDomainCMGOAttributeName.
	aAttributeObjectDomain isNil ifTrue: [ ^self].

	aAttributeObjectDomain object: theNewObject setTC: theDomainCMGO.!

findHomeInDomain: theDomainCMGO

	| aDomainMetaInfo aDomainModel aTypeDomain aTypeHome aRelationshipHomes someHomes aHomeCMGO aAttributeHomedElementsTypeMetaInfo |
	theDomainCMGO isNil ifTrue: [ ^nil].

	aDomainMetaInfo := theDomainCMGO metaInfo.
	aDomainMetaInfo isNil ifTrue: [ ^nil].

	aDomainModel := aDomainMetaInfo model.
	aDomainModel isNil ifTrue: [ ^nil].

	aTypeDomain := aDomainModel resolveReferencedTypeName: self class domainCMGOTypeName 
		moduleNames: self class domainCMGOModuleNames.
	aTypeDomain isNil ifTrue: [ ^nil].

	aDomainMetaInfo == aTypeDomain ifFalse: [ ^nil].
	
	aTypeHome := aDomainModel resolveReferencedTypeName: self class homeCMGOTypeName 
		moduleNames: self class homeCMGOModuleNames.
	aTypeHome isNil ifTrue: [ ^nil].

	aRelationshipHomes := aDomainMetaInfo relationshipNamed: self class homesCMGODomainRelationshipName.
	aRelationshipHomes isNil ifTrue: [ ^nil].

	someHomes := aRelationshipHomes getObjectFeatureValueTC: theDomainCMGO.
	(someHomes isNil or: [ someHomes isEmpty]) ifTrue: [ ^nil].

	aAttributeHomedElementsTypeMetaInfo := aTypeHome 
		attributeOrInheritedNamed:  self class homedElementsTypeMetaInfoCMGODomainAttributeName.
	aAttributeHomedElementsTypeMetaInfo isNil ifTrue: [ ^nil].

	aHomeCMGO := someHomes detect: [:aHome |   | aHomedElementsTypeMetaInfo  |
		aHomedElementsTypeMetaInfo := aAttributeHomedElementsTypeMetaInfo getObjectFeatureValue: aHome.
		aHomedElementsTypeMetaInfo isNil not and: [ aHomedElementsTypeMetaInfo == self]
	] ifNone: [ nil].
	^aHomeCMGO!

getHomeForIdCounterInDomain: theDomain

	| aHome |

	theDomain isNil ifTrue: [ ^nil].

	(self isHomeRooted and: [ self isHomeIDProvider]) ifFalse: [ ^nil].

	aHome := self findHomeInDomain: theDomain.
	^aHome!

requiresIdCounter: theObject
	
	| someEffectiveAttributes aNonVirtualType aObjectMetaInfo |

	theObject isNil ifTrue: [ ^nil].

	self isPrimitive ifTrue: [ ^false].
	self isEnumeration ifTrue: [ ^false].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^false].
	
	aNonVirtualType isPrimitive ifTrue: [ ^false].
	aNonVirtualType isEnumeration ifTrue: [ ^false].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^false].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^false].
	
	someEffectiveAttributes := aObjectMetaInfo allEffectiveAttributes.
	(someEffectiveAttributes isNil or: [ someEffectiveAttributes isEmpty]) ifTrue: [ ^false].

	 ^(someEffectiveAttributes detect: [:anAttribute |  anAttribute requiresIdCounter: theObject] ifNone: [ nil]) isNil not!

searchDomainFrom: theObject

	| aMetaInfo someAggregatedIntoRelationships aDomain |

	theObject isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aDomain := self getObjectDomainValue:  theObject.
	aDomain isNil ifFalse: [ ^aDomain].
  
	someAggregatedIntoRelationships := aMetaInfo allEffectiveAggregatedIntoRelationships.
	(someAggregatedIntoRelationships isNil or: [ someAggregatedIntoRelationships isEmpty]) ifTrue: [ ^nil].

	aDomain := nil.
	someAggregatedIntoRelationships detect: [:aRelationship |
		aDomain := aRelationship searchDomainFrom: theObject.
		aDomain isNil not
	] ifNone: [ nil].

	^aDomain!

searchHomeForIdCounterFrom: theObject inDomain: theDomain

	| aMetaInfo someAggregatedIntoRelationships aHome |

	theObject isNil ifTrue: [ ^nil].
	theDomain isNil ifTrue: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aHome := self getHomeForIdCounterInDomain: theDomain.
	aHome isNil ifFalse: [ ^aHome].


	someAggregatedIntoRelationships := aMetaInfo allEffectiveAggregatedIntoRelationships.
	(someAggregatedIntoRelationships isNil or: [ someAggregatedIntoRelationships isEmpty]) ifTrue: [ ^nil].

	aHome := nil.
	someAggregatedIntoRelationships detect: [:aRelationship |
		aHome := aRelationship searchHomeForIdCounterFrom: theObject inDomain: theDomain.
		aHome isNil not
	] ifNone: [ nil].

	^aHome! !

!CODEType publicMethodsFor: 'TRF-dynamic metainfo'!

createNonPersistentNLSFor: theSpecializedElement  original: theOriginalTranslationElement

	| aOriginalTranslationItem aNewTranslationItem aModel aTranslation |

	theSpecializedElement isNil ifTrue: [ ^nil].
	theOriginalTranslationElement isNil  ifTrue: [ ^nil].
	

	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aTranslation := aModel nlsSolver.
	(aTranslation isNil or: [ aTranslation isVoidTranslation]) ifTrue: [ ^nil].

	(aTranslation  
			nlsFirstResolverItemGroupNoDefault: theOriginalTranslationElement nameNLSGroupName 
			item: theOriginalTranslationElement nameNLSItemName) isNil ifTrue: [ ^nil].

	(aTranslation nlsLocalResolverItemGroupNoDefault: theSpecializedElement nameNLSGroupName 
		item: theSpecializedElement nameNLSItemName) isNil ifTrue: [ 

		aOriginalTranslationItem := aTranslation  
			nlsFirstResolverItemGroupNoDefault: theOriginalTranslationElement nameNLSGroupName 
			item: theOriginalTranslationElement nameNLSItemName.
		aOriginalTranslationItem isNil ifFalse: [

			aTranslation 
				recordNonPersistentNLSGroup: theSpecializedElement nameNLSGroupName 
				item: theSpecializedElement nameNLSItemName.

			aNewTranslationItem := aTranslation nlsLocalResolverItemGroupNoDefault: theSpecializedElement  nameNLSGroupName
				item: theSpecializedElement nameNLSItemName.
			aNewTranslationItem isNil ifFalse: [
				aNewTranslationItem usedItemTranslationsAdd: aOriginalTranslationItem
			]
		]
	].!

createNonPersistentNLSFor: theSpecializedElement  originals: theOriginalTranslationElements translation: theTranslation

	| aNewTranslationItem aModel aTranslation |

	theSpecializedElement isNil ifTrue: [ ^nil].
	(theOriginalTranslationElements isNil or: [ theOriginalTranslationElements isEmpty]) ifTrue: [ ^nil].


		aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aTranslation := aModel nlsSolver.
	(aTranslation isNil or: [ aTranslation isVoidTranslation]) ifTrue: [ ^nil].

	(theOriginalTranslationElements reject: [:aOriginalTranslationElement |  
		(aTranslation  
			nlsFirstResolverItemGroupNoDefault: aOriginalTranslationElement nameNLSGroupName 
			item: aOriginalTranslationElement nameNLSItemName) isNil
	]) isEmpty ifTrue: [ ^nil].

	(aTranslation nlsLocalResolverItemGroupNoDefault: theSpecializedElement nameNLSGroupName 
		item: theSpecializedElement nameNLSItemName) isNil ifTrue: [ 

		(theTranslation isNil or: [ theTranslation isEmpty])
			ifFalse: [ 
				aTranslation 
					recordNonPersistentNLSGroup: theSpecializedElement nameNLSGroupName 
					item: theSpecializedElement nameNLSItemName
					translation: theTranslation
			]
			ifTrue: [ 
				aTranslation 
					recordNonPersistentNLSGroup: theSpecializedElement nameNLSGroupName 
					item: theSpecializedElement nameNLSItemName
			].

		aNewTranslationItem := aTranslation nlsLocalResolverItemGroupNoDefault: theSpecializedElement  nameNLSGroupName
			item: theSpecializedElement nameNLSItemName.
		aNewTranslationItem isNil ifFalse: [
			theOriginalTranslationElements do: [:aOriginalTranslationElement |  | aOriginalTranslationItem |
				aOriginalTranslationItem := aTranslation  
					nlsFirstResolverItemGroupNoDefault: aOriginalTranslationElement nameNLSGroupName 
					item: aOriginalTranslationElement nameNLSItemName.
				aOriginalTranslationItem isNil ifFalse: [
					aNewTranslationItem usedItemTranslationsAdd: aOriginalTranslationItem
				]
			]
		]
	]!

requiredFeatureNameFromPrefixes: thePrefixes

	| aOriginalRequiredFeature aNamePrefix aStream aNewRequiredAttributeName |

	(thePrefixes isNil or: [ thePrefixes isEmpty]) ifTrue: [ ^nil].

	aOriginalRequiredFeature := thePrefixes last.
	aOriginalRequiredFeature isRelationship ifTrue: [ ^nil].

	aNamePrefix := thePrefixes size > 1
		ifFalse: [ '']
		ifTrue: [ 
			thePrefixes size > 2
				ifFalse: [ thePrefixes first name copy, ' ']
				ifTrue: [ 
					aStream := WriteStream on: (String new: thePrefixes size * 32).
					(thePrefixes copyFrom: 1 to: thePrefixes size - 1) do: [:aFeature | 
						aFeature == thePrefixes first ifFalse: [ aStream nextPutAll: '.'].
						aStream nextPutAll: aFeature name
					].
					aStream contents, ' '
				]
		].
	aNewRequiredAttributeName := aNamePrefix,  aOriginalRequiredFeature name.
	^aNewRequiredAttributeName!

requiredFeaturesTypeMetaInfo

	| aModel someModuleNames aModule someBaseModuleNames aRequiredFeaturesTypeName someTypeModuleNames aRequiredFeaturesType someFeatures aRequiredFeaturesParentAttributeName aRequiredFeaturesParentAttribute aGeneralObjectType someAggregatedIntoRelationships |

	self isAbstract ifTrue: [ ^nil].

	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	someBaseModuleNames := self class requiredFeaturesTypesModuleNames.
	aRequiredFeaturesTypeName := self name, self class requiredFeaturesTypePostfix.
	someTypeModuleNames := self module fullyQualifiedNameArray.

	someModuleNames := someBaseModuleNames , someTypeModuleNames.

	aRequiredFeaturesType := aModel resolveReferencedTypeName: aRequiredFeaturesTypeName moduleNames: someModuleNames.
	aRequiredFeaturesType isNil ifFalse: [ 
		^aRequiredFeaturesType attributes isEmpty ifTrue: [ nil] ifFalse: [ aRequiredFeaturesType]
	].

	aModule := aModel.
	1 to: someModuleNames size do: [:aLength |  |  aNewModule aModuleNames |
		aModuleNames := someModuleNames copyFrom: 1 to: aLength.
		aNewModule := aModel resolveReferencedModuleNames: aModuleNames.
		aNewModule isNil 
			ifFalse: [ aModule := aNewModule]
			ifTrue: [ 
				aNewModule := self preferredModuleClass new name: aModuleNames last.
				aNewModule isNotPersistentMetaInfo: true.
				aModule subModulesAdd: aNewModule.
				aModule := aNewModule.
			]
	].
	
	aRequiredFeaturesType := aModule typeNamed: aRequiredFeaturesTypeName.
(true or: [ aRequiredFeaturesType isNil]) ifFalse: [ ^aRequiredFeaturesType "Should never happen as it is attempted to resolve up in the method" ].

	aRequiredFeaturesType := self preferredTypeClass new.
	aRequiredFeaturesType name: aRequiredFeaturesTypeName.
	aRequiredFeaturesType isNotPersistentMetaInfo: true.
	aRequiredFeaturesType isRequiredFeaturesType: true.
	aModule typesAdd: aRequiredFeaturesType.

	self createNonPersistentNLSFor: aRequiredFeaturesType  originals: (Array with: self)  translation: 'Creacion %1'.

	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^self].

	someFeatures do: [:aFeature |
		aFeature requiredFeaturesTypeMetaInfo: aRequiredFeaturesType prefixes: Array new
	].

	aRequiredFeaturesType attributes isEmpty ifTrue: [ ^nil].

	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aGeneralObjectType := aModel resolveReferencedTypeName: self class CMGOTypeName 
		moduleNames: self class CMGOTypeModuleNames.
	aGeneralObjectType isNil ifTrue: [ ^nil].

	aRequiredFeaturesParentAttributeName := self class requiredFeaturesParentAttributeName.
	aRequiredFeaturesParentAttribute := self preferredAttributeClass new.
	aRequiredFeaturesParentAttribute name: aRequiredFeaturesParentAttributeName.
	aRequiredFeaturesParentAttribute isNotPersistentMetaInfo: true.
	aRequiredFeaturesParentAttribute minMult: self class minMultOptional.
	aRequiredFeaturesParentAttribute maxMult: self class maxMultOne.
	aRequiredFeaturesParentAttribute isAbstract: false.
	aRequiredFeaturesParentAttribute valueType: aGeneralObjectType.
	aRequiredFeaturesParentAttribute isChangeable: false.
	"aRequiredFeaturesParentAttribute computationKind: aRequiredFeaturesParentAttribute class computationKindInitialValue.
	aRequiredFeaturesParentAttribute initializationExpression: self class initExpressionLiteralValue , ' ', self class initExpressionNilValue."
	aRequiredFeaturesParentAttribute isInitializationPropagationAllowed: false.
	aRequiredFeaturesParentAttribute isInitializationPropagationOnConnectAllowed: false.

	aRequiredFeaturesType attributesAdd: aRequiredFeaturesParentAttribute.

	someAggregatedIntoRelationships := self allEffectiveAggregatedIntoRelationships.
	(someAggregatedIntoRelationships isNil or: [ someAggregatedIntoRelationships isEmpty]) ifFalse: [

		someAggregatedIntoRelationships do: [:aRelationship |  | aRequiredFeaturesAggregatedIntoAttributeName aRequiredFeaturesAggregatedIntoAttribute |
			aRequiredFeaturesAggregatedIntoAttributeName := aRelationship name.
			aRequiredFeaturesAggregatedIntoAttribute := self preferredAttributeClass new.
			aRequiredFeaturesAggregatedIntoAttribute name: aRequiredFeaturesAggregatedIntoAttributeName.
			aRequiredFeaturesAggregatedIntoAttribute isNotPersistentMetaInfo: true.
			aRequiredFeaturesAggregatedIntoAttribute minMult: self class minMultOptional.
			aRequiredFeaturesAggregatedIntoAttribute maxMult: self class maxMultOne.
			aRequiredFeaturesAggregatedIntoAttribute isAbstract: false.
			aRequiredFeaturesAggregatedIntoAttribute valueType: aRelationship relatedType.
			aRequiredFeaturesAggregatedIntoAttribute isChangeable: false.
			aRequiredFeaturesAggregatedIntoAttribute isInitializationPropagationAllowed: false.
			aRequiredFeaturesAggregatedIntoAttribute isInitializationPropagationOnConnectAllowed: false.
			aRequiredFeaturesType attributesAdd: aRequiredFeaturesAggregatedIntoAttribute.
		]
	].

 	^aRequiredFeaturesType!

requiredFeaturesTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes

	| aNonVirtualType someFeatures aOriginalRequiredFeature aStream aNewRequiredAttributeName aNewRequiredAttribute aTranslation someAspects |

	self isAbstract ifTrue: [ ^nil].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^nil].
	
	(self isPrimitive or: [ self isEnumeration or:  [ aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]]]) ifTrue: [ 

		thePrefixes isEmpty ifTrue: [ ^self].

		aOriginalRequiredFeature := thePrefixes last.
		aOriginalRequiredFeature isRelationship ifTrue: [ ^self "Should never happen"].

		aNewRequiredAttributeName := self requiredFeatureNameFromPrefixes: thePrefixes.
		aNewRequiredAttribute := self preferredAttributeClass new.
		aNewRequiredAttribute name: aNewRequiredAttributeName.
		aNewRequiredAttribute isNotPersistentMetaInfo: true.

		aNewRequiredAttribute initTerminalMetaAttributesFrom: aOriginalRequiredFeature.
		aNewRequiredAttribute maxMult: self class maxMultOne.
		aNewRequiredAttribute valueType: self.
		aNewRequiredAttribute isChangeable: true.
	
		(aOriginalRequiredFeature computationKind = self class computationKindInitialValue or: [
 			aOriginalRequiredFeature computationKind = self class computationKindInitializedInConstructor and: [
				aOriginalRequiredFeature initializationExpression isEmpty not
			]]) ifTrue: [ 
				aNewRequiredAttribute computationKind: aNewRequiredAttribute class computationKindInitialValue.
				aNewRequiredAttribute initializationExpression: aNewRequiredAttribute initializationExpression.		
			]
			ifFalse: [	aNewRequiredAttribute computationKind: aNewRequiredAttribute class computationKindNoComputation.].

		
		theRequiredFeaturesType attributesAdd: aNewRequiredAttribute.

		someAspects := aOriginalRequiredFeature aspects.
		(someAspects isNil or: [ someAspects isEmpty]) ifFalse: [ 
			someAspects do: [:anAspect |  aNewRequiredAttribute aspectsAdd: anAspect]
		].
		aStream := WriteStream on: (String new: thePrefixes size * 32).
		1 to: thePrefixes size do: [:anIndex | 
			anIndex = 1 ifFalse: [ aStream nextPutAll: '.'].
			aStream nextPutAll: '%'; print: anIndex
		].
		aTranslation := aStream contents.
		self createNonPersistentNLSFor: aNewRequiredAttribute  originals: thePrefixes asArray  translation: aTranslation.

		^self
	].
	


	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^self].

	someFeatures do: [:aFeature |
		aFeature requiredFeaturesTypeMetaInfo: theRequiredFeaturesType prefixes: thePrefixes
	].!

specializedHomeMetaInfoForHomeElementsType: theHomeElementsType

	| aModel someModuleNames aSpecializedTypeName aSpecializedType aModule aHomeRootsAttribute aSpecializedAttribute |

	theHomeElementsType isNil ifTrue: [ ^nil].
	
	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aHomeRootsAttribute := self attributeOrInheritedNamed: self class homeRootsCMGODomainAttributeName.
	aHomeRootsAttribute isNil ifTrue: [ ^nil].

	someModuleNames := self class specializedHomeTypesModuleNames.
	aSpecializedTypeName := theHomeElementsType name , '_', self name.
	
	aSpecializedType := aModel resolveReferencedTypeName: aSpecializedTypeName moduleNames: someModuleNames.
	aSpecializedType isNil ifFalse: [ ^aSpecializedType].

	aModule := aModel.
	1 to: someModuleNames size do: [:aLength |  |  aNewModule aModuleNames |
		aModuleNames := someModuleNames copyFrom: 1 to: aLength.
		aNewModule := aModel resolveReferencedModuleNames: aModuleNames.
		aNewModule isNil 
			ifFalse: [ aModule := aNewModule]
			ifTrue: [ 
				aNewModule := self preferredModuleClass new name: aModuleNames last.
				aNewModule isNotPersistentMetaInfo: true.
				aModule subModulesAdd: aNewModule.
				aModule := aNewModule.
			]
	].
	
	aSpecializedType := aModule typeNamed: aSpecializedTypeName.
	aSpecializedType isNil ifFalse: [ ^aSpecializedType "Should never happen as it is attempted to resolve up in the method" ].

	aSpecializedType := self preferredTypeClass new.
	aSpecializedType name: aSpecializedTypeName.
	aSpecializedType isNotPersistentMetaInfo: true.
	aModule typesAdd: aSpecializedType.
	aSpecializedType superTypesAdd: self.

	aSpecializedAttribute := self preferredAttributeRefinementClass new.
	aSpecializedAttribute name: aHomeRootsAttribute name.
	aSpecializedAttribute isNotPersistentMetaInfo: true.
	aSpecializedAttribute refinedAttributesAdd: aHomeRootsAttribute.
	aSpecializedAttribute initTerminalMetaAttributesFrom: aHomeRootsAttribute.

	aSpecializedType attributesAdd: aSpecializedAttribute.
	aSpecializedAttribute valueType: theHomeElementsType.


	self createNonPersistentNLSFor: aSpecializedType  originals: (Array with: theHomeElementsType with: self)  translation: '%2 %1'.
	self createNonPersistentNLSFor: aSpecializedAttribute  original: theHomeElementsType.

	^aSpecializedType!

validateRequiredFeaturesObject: theRequiredFeaturesObject

	| someFeatures someIssues |
	theRequiredFeaturesObject isNil ifTrue: [ ^nil].

	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^nil].

	someIssues := OrderedCollection new: someFeatures size.
	someFeatures do: [:aFeature |  |  anIssue  |
 		anIssue := aFeature validateRequiredFeaturesObject: theRequiredFeaturesObject.
		(anIssue isNil not and: [ anIssue isEmpty not]) ifTrue: [ 
			someIssues add: anIssue
		]
	].

	someIssues isEmpty ifTrue: [ ^nil].
	^someIssues! !

!CODEType publicMethodsFor: 'TRF-feature accessing'!

addObject: theObject featureNamed: theFeatureName value: theValue
	| aObjectMetaInfo aFeature aFeatureValue |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature object: theObject addTC: theValue.
	^aFeatureValue!

addToObject: theObject featureNamedValue: theFeatureName create: theCreationFeaturesAndValues

	| aObjectMetaInfo aFeature aReferencedType aNonVirtualReferencedType aNewObject  |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aReferencedType := aFeature referencedType.
	aReferencedType  isNil ifTrue: [ ^nil].

	aNonVirtualReferencedType := aReferencedType nonVirtualType.
	aNonVirtualReferencedType  isNil ifTrue: [ ^nil].
	
	(aNonVirtualReferencedType isPrimitive or: [ aNonVirtualReferencedType isEnumeration]) ifTrue: [ 
		^nil "should not try to create on inner features of Primitives".
	].

	aNewObject := aReferencedType createObject.
	aNewObject  isNil ifTrue: [ ^nil].

	(theCreationFeaturesAndValues isNil not and: [ theCreationFeaturesAndValues isEmpty not]) ifTrue: [ 
		theCreationFeaturesAndValues do: [:aFeatureAndValue |   | aInitFeatureName aInitFeatureValue aInitFeature |
			(aFeatureAndValue isNil not and: [ aFeatureAndValue size > 1]) ifTrue: [ 
				aInitFeatureName := aFeatureAndValue first.
				aInitFeatureValue := aFeatureAndValue at: 2.
				(aInitFeatureName isNil not and: [ aInitFeatureValue isNil not]) ifTrue: [ 
					aInitFeature := aReferencedType effectiveFeatureNamed: aInitFeatureName.
					aInitFeature isNil 
						ifTrue: [ DEBUGDvpt ifTrue: [ self halt]] 
						ifFalse: [ aInitFeature object: aNewObject setTC: aInitFeatureValue]
				]
			]
		]
	].

	aFeature isMultiplicityMany
		ifTrue: [ aFeature object:  theObject addTC: aNewObject]
		ifFalse: [ aFeature object:  theObject setTC: aNewObject].

	^aNewObject!

addToObject: theObject featureNamedValue: theFeatureName factoryNamed: theFactoryName create: theCreationFeaturesAndValues

	| aObjectMetaInfo aFeature aReferencedType aNonVirtualReferencedType aNewObject  aFactoryType |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aReferencedType := aFeature referencedType.
	aReferencedType  isNil ifTrue: [ ^nil].

	aNonVirtualReferencedType := aReferencedType nonVirtualType.
	aNonVirtualReferencedType  isNil ifTrue: [ ^nil].
	
	(aNonVirtualReferencedType isPrimitive or: [ aNonVirtualReferencedType isEnumeration]) ifTrue: [ 
		^nil "should not try to create on inner features of Primitives".
	].

	aFactoryType := aReferencedType.
	(theFactoryName isNil not and: [ theFactoryName isEmpty not]) ifTrue: [ 
		aFactoryType := self model typeNamed: theFactoryName.
		aFactoryType isNil ifTrue: [ ^nil]
	].

	aNewObject := aFactoryType createObject.
	aNewObject  isNil ifTrue: [ ^nil].

	(theCreationFeaturesAndValues isNil not and: [ theCreationFeaturesAndValues isEmpty not]) ifTrue: [ 
		theCreationFeaturesAndValues do: [:aFeatureAndValue |   | aInitFeatureName aInitFeatureValue aInitFeature |
			(aFeatureAndValue isNil not and: [ aFeatureAndValue size > 1]) ifTrue: [ 
				aInitFeatureName := aFeatureAndValue first.
				aInitFeatureValue := aFeatureAndValue at: 2.
				(aInitFeatureName isNil not and: [ aInitFeatureValue isNil not]) ifTrue: [ 
					aInitFeature := aFactoryType effectiveFeatureNamed: aInitFeatureName.
					aInitFeature isNil ifFalse: [ 
						aInitFeature object: aNewObject setTC: aInitFeatureValue.	
					]
				]
			]
		]
	].

	aFeature isMultiplicityMany
		ifTrue: [ aFeature object:  theObject addTC: aNewObject]
		ifFalse: [ aFeature object:  theObject setTC: aNewObject].

	^aNewObject!

getObject: theObject expression: theExpression

	^self computeDerivedValueFrom: theObject expression: theExpression!

getObject: theObject featureNamedValue: theFeatureName
	| aObjectMetaInfo aFeature aFeatureValue |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature getObjectFeatureValueTC: theObject.
	^aFeatureValue!

getObject: theObject featureNamedValue: theFeatureName detect: theSelectFeatureName test: theTestBlock
	| aObjectMetaInfo aFeature aFeatureValue aReferencedType aNonVirtualReferencedType aTestBlock someFeatureValues aTestResult aSelectFeatureName aNameFeature aFValueMetaInfo aSelectFeature aSelectionFeatureValue aSingleObject aSObjectMetaInfo aSOSelectFeature |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature getObjectFeatureValueTC: theObject.
	aFeatureValue  isNil ifTrue: [ ^nil].

	aReferencedType := aFeature referencedType.
	aReferencedType  isNil ifTrue: [ ^nil].

	aNonVirtualReferencedType := aReferencedType nonVirtualType.
	aNonVirtualReferencedType  isNil ifTrue: [ ^nil].
	
	aTestBlock := theTestBlock isNil ifFalse: [ theTestBlock] ifTrue: [   [:aVal | true]   ].

	(aNonVirtualReferencedType isPrimitive or: [ aNonVirtualReferencedType isEnumeration]) ifTrue: [ 
		(theSelectFeatureName isNil or: [ theSelectFeatureName isEmpty]) ifFalse: [ 
			^nil "should not try to filter on inner features of Primitives".
		].
		^aFeature isMultiplicityMany
			ifTrue: [ 
				someFeatureValues := aFeatureValue.
				someFeatureValues isEmpty 
					ifTrue: [ nil]
					ifFalse:[ someFeatureValues detect: [:aFValue | theTestBlock value: aFValue ]  ifNone: [ nil] ]
			]
			ifFalse: [ 
				aTestResult := aTestBlock value: aFeatureValue.
				aTestResult ifTrue: [ aFeatureValue] ifFalse: [ nil]
			]
	].
	 
	aSelectFeatureName := (theSelectFeatureName isNil or: [ theSelectFeatureName isEmpty]) 
		ifFalse: [ theSelectFeatureName]
		ifTrue: [  
			aNameFeature := aReferencedType nameAttribute.
			aNameFeature isNil ifTrue: [ nil] ifFalse: [ aNameFeature name]
		].
	aSelectFeatureName isNil ifTrue: [ ^nil].

	^aFeature isMultiplicityMany
		ifTrue: [ 
			someFeatureValues := aFeatureValue.
			someFeatureValues isEmpty 
				ifTrue: [ nil]
				ifFalse: [ 
					someFeatureValues detect: [:aFValue | 
						aFValueMetaInfo := aFValue metaInfo.
						aFValueMetaInfo isNil 
							ifTrue: [ false] 
							ifFalse: [ 
								aSelectFeature := aFValueMetaInfo effectiveFeatureNamed: aSelectFeatureName.
								aSelectFeature isNil 
									ifTrue: [ false] 
									ifFalse: [ 
										aSelectionFeatureValue := aSelectFeature getObjectFeatureValueTC: aFValue.
										aTestBlock value: aSelectionFeatureValue
									]
							]
					]  ifNone: [ nil]
				]
		]
		ifFalse: [ 
			aSingleObject := aFeatureValue.
			aSObjectMetaInfo := aSingleObject metaInfo.
			aSObjectMetaInfo isNil 
				ifTrue: [ nil] 
				ifFalse: [ 
					aSOSelectFeature := aSObjectMetaInfo effectiveFeatureNamed: aSelectFeatureName.
					aSOSelectFeature isNil 
						ifTrue: [ nil] 
						ifFalse: [ 
							aSelectionFeatureValue := aSOSelectFeature getObjectFeatureValueTC: aSingleObject.
							(aTestBlock value: aSelectionFeatureValue) ifTrue: [ aSingleObject] ifFalse: [ nil]
						]
				]
		]!

getObject: theObject featureNamedValue: theFeatureName detect: theSelectFeatureName test: theTestBlock 
	orCreate: theCreationFeatureValue

	| aObjectMetaInfo aFeature aReferencedType aNonVirtualReferencedType aNameFeature anExistingObject aNewObject aInitFeatureName aInitFeature |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	anExistingObject := self getObject: theObject featureNamedValue: theFeatureName detect: theSelectFeatureName test: theTestBlock.
	anExistingObject isNil ifFalse: [ ^anExistingObject].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aReferencedType := aFeature referencedType.
	aReferencedType  isNil ifTrue: [ ^nil].

	aNonVirtualReferencedType := aReferencedType nonVirtualType.
	aNonVirtualReferencedType  isNil ifTrue: [ ^nil].
	
	(aNonVirtualReferencedType isPrimitive or: [ aNonVirtualReferencedType isEnumeration]) ifTrue: [ 
			^nil "should not try to create on inner features of Primitives".
	].

	aNewObject := aReferencedType createObject.
	aNewObject  isNil ifTrue: [ ^nil].

	aInitFeatureName := (theSelectFeatureName isNil or: [ theSelectFeatureName isEmpty]) 
		ifFalse: [ theSelectFeatureName]
		ifTrue: [  
			aNameFeature := aReferencedType nameAttribute.
			aNameFeature isNil ifTrue: [ nil] ifFalse: [ aNameFeature name]
		].
	aInitFeatureName isNil ifTrue: [ ^aNewObject].

	aInitFeature := aReferencedType effectiveFeatureNamed: aInitFeatureName.
	aInitFeature isNil ifTrue: [ ^aNewObject].

	aInitFeature object: aNewObject setTC: theCreationFeatureValue.

	aFeature isMultiplicityMany
		ifTrue: [ aFeature object:  theObject addTC: aNewObject]
		ifFalse: [ aFeature object:  theObject setTC: aNewObject].

	^aNewObject!

getObject: theObject featureNamedValue: theFeatureName orCreate: theCreationFeaturesAndValues

	| anExistingObject |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	anExistingObject := self getObject: theObject featureNamedValue: theFeatureName.
	anExistingObject isNil ifFalse: [ ^anExistingObject].

	^self addToObject: theObject featureNamedValue: theFeatureName create: theCreationFeaturesAndValues!

getObject: theObject featureNamedValue: theFeatureName select: theSelectFeatureName test: theTestBlock
	| aObjectMetaInfo aFeature aFeatureValue aReferencedType aNonVirtualReferencedType aTestBlock someFeatureValues aTestResult aSelectFeatureName aNameFeature aFValueMetaInfo aSelectFeature aSelectionFeatureValue aSingleObject aSObjectMetaInfo aSOSelectFeature |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature getObjectFeatureValueTC: theObject.
	aFeatureValue  isNil ifTrue: [ ^nil].

	aReferencedType := aFeature referencedType.
	aReferencedType  isNil ifTrue: [ ^nil].

	aNonVirtualReferencedType := aReferencedType nonVirtualType.
	aNonVirtualReferencedType  isNil ifTrue: [ ^nil].
	
	aTestBlock := theTestBlock isNil ifFalse: [ theTestBlock] ifTrue: [   [:aVal | true]   ].

	(aNonVirtualReferencedType isPrimitive or: [ aNonVirtualReferencedType isEnumeration]) ifTrue: [ 
		(theSelectFeatureName isNil or: [ theSelectFeatureName isEmpty]) ifFalse: [ 
			^nil "should not try to filter on inner features of Primitives".
		].
		^aFeature isMultiplicityMany
			ifTrue: [ 
				someFeatureValues := aFeatureValue.
				someFeatureValues isEmpty 
					ifTrue: [ nil]
					ifFalse:[ someFeatureValues select: [:aFValue | theTestBlock value: aFValue ]   ]
			]
			ifFalse: [ 
				aTestResult := aTestBlock value: aFeatureValue.
				aTestResult ifTrue: [ aFeatureValue] ifFalse: [ nil]
			]
	].
	 
	aSelectFeatureName := (theSelectFeatureName isNil or: [ theSelectFeatureName isEmpty]) 
		ifFalse: [ theSelectFeatureName]
		ifTrue: [  
			aNameFeature := aReferencedType nameAttribute.
			aNameFeature isNil ifTrue: [ nil] ifFalse: [ aNameFeature name]
		].
	aSelectFeatureName isNil ifTrue: [ ^nil].

	^aFeature isMultiplicityMany
		ifTrue: [ 
			someFeatureValues := aFeatureValue.
			someFeatureValues isEmpty ifTrue: [ ^nil].
			someFeatureValues select: [:aFValue | 
				aFValueMetaInfo := aFValue metaInfo.
				aFValueMetaInfo isNil 
					ifTrue: [ false] 
					ifFalse: [ 
						aSelectFeature := aFValueMetaInfo effectiveFeatureNamed: aSelectFeatureName.
						aSelectFeature isNil 
							ifTrue: [ false] 
							ifFalse: [ 
self halt.
								aSelectionFeatureValue := aSelectFeature getObjectFeatureValueTC: aFValue.
								aTestBlock value: aSelectionFeatureValue
							]
					]
			]
		]
		ifFalse: [ 
			aSingleObject := aFeatureValue.
			aSObjectMetaInfo := aSingleObject metaInfo.
			aSObjectMetaInfo isNil 
				ifTrue: [ nil] 
				ifFalse: [ 
					aSOSelectFeature := aSObjectMetaInfo effectiveFeatureNamed: aSelectFeatureName.
					aSOSelectFeature isNil 
						ifTrue: [ nil] 
						ifFalse: [ 
							aSelectionFeatureValue := aSOSelectFeature getObjectFeatureValueTC: aSingleObject.
							(aTestBlock value: aSelectionFeatureValue) ifTrue: [ aSingleObject] ifFalse: [ nil]
						]
				]
		]!

performObject: theObject operationNamed: theOperationName
	| aObjectMetaInfo aFeature aFeatureValue |
	theObject isNil ifTrue: [ ^nil].
	theOperationName isNil ifTrue: [ ^nil].
self halt.
	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveOperationNamed: theOperationName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature performObjectOperationTC: theObject.
	^aFeatureValue!

performObject: theObject operationNamed: theOperationName argumentsHolder: theArgumentsHolder
	| aObjectMetaInfo aFeature aFeatureValue |
	theObject isNil ifTrue: [ ^nil].
	theOperationName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveOperationNamed: theOperationName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature performObjectOperationTC: theObject  argumentsHolder: theArgumentsHolder.
	^aFeatureValue!

removeObject: theObject featureNamed: theFeatureName value: theValue
	| aObjectMetaInfo aFeature aFeatureValue |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature object: theObject removeTC: theValue.
	^aFeatureValue!

setObject: theObject expression: theExpression value: theValue!

setObject: theObject featureNamed: theFeatureName value: theValue
	| aObjectMetaInfo aFeature aFeatureValue |
	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature object: theObject setTC: theValue.
	^aFeatureValue!

unsetObject: theObject featureNamed: theFeatureName value: theValue
	| aObjectMetaInfo aFeature aFeatureValue |

	theObject isNil ifTrue: [ ^nil].
	theFeatureName isNil ifTrue: [ ^nil].
	theValue isNil ifTrue: [ ^nil].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].

	aObjectMetaInfo := theObject metaInfo.
	aObjectMetaInfo isNil ifTrue: [ ^nil].

	aFeature := aObjectMetaInfo effectiveFeatureNamed: theFeatureName.
	aFeature  isNil ifTrue: [ ^nil].

	aFeatureValue := aFeature object: theObject unsetTC: theValue.
	^aFeatureValue! !

!CODEType publicMethodsFor: 'TRF-homes'!

createHomeObjectInDomain: theDomain
	| aModel aHomeCMGO aMetaInfo aTypeDomain aTypeHome aRelationshipHomes aAttributeHomedElementsTypeMetaInfo aAttributeHomeName aAttributeHomeIDCounter aHasIdCounter aSpecializedHomeMetaInfo |

	theDomain isNil ifTrue: [ ^nil].

	self isAbstract ifTrue: [ ^nil].

	aHasIdCounter := self hasIdCounter.
	(self isHomeRooted not and: [ aHasIdCounter not]) ifTrue: [ ^nil].

	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aMetaInfo := theDomain metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].
	
	aTypeDomain := aModel resolveReferencedTypeName: self class domainCMGOTypeName 
		moduleNames: self class domainCMGOModuleNames.
	aTypeDomain isNil ifTrue: [ ^nil].

	aMetaInfo == aTypeDomain ifFalse: [ ^nil].
	
	aTypeHome := aModel resolveReferencedTypeName: self class homeCMGOTypeName moduleNames: self class homeCMGOModuleNames.
	aTypeHome isNil ifTrue: [ ^nil].

	aHomeCMGO := aTypeHome createObject.

	aAttributeHomeName := aTypeHome attributeOrInheritedNamed:  self class homeNameCMGODomainAttributeName.
	aAttributeHomeName isNil ifTrue: [ ^nil].

	aAttributeHomeName object: aHomeCMGO setTC: self name copy.

	aHasIdCounter ifTrue: [ 
		aAttributeHomeIDCounter := aTypeHome attributeOrInheritedNamed:  self class homeIDCounterCMGODomainAttributeName.
		aAttributeHomeIDCounter isNil ifTrue: [ ^nil].

		aAttributeHomeIDCounter object: aHomeCMGO setTC: 1
	].

	aAttributeHomedElementsTypeMetaInfo := aTypeHome attributeOrInheritedNamed:  self class homedElementsTypeMetaInfoCMGODomainAttributeName.
	aAttributeHomedElementsTypeMetaInfo isNil ifTrue: [ ^nil].

	aAttributeHomedElementsTypeMetaInfo object: aHomeCMGO setTC: self.

	aSpecializedHomeMetaInfo := aTypeHome specializedHomeMetaInfoForHomeElementsType: self.
	aSpecializedHomeMetaInfo isNil ifFalse: [ 
		aHomeCMGO forzeMetaInfo: aSpecializedHomeMetaInfo.
	].

	aRelationshipHomes := aTypeDomain relationshipNamed: self class homesCMGODomainRelationshipName.
	aRelationshipHomes isNil ifTrue: [ ^nil].

	aRelationshipHomes object: theDomain addTC: aHomeCMGO.!

createSpecializedHomeType
	| aModel aTypeHome aHasIdCounter aSpecializedHomeMetaInfo |

	self isAbstract ifTrue: [ ^nil].

	aHasIdCounter := self hasIdCounter.
	(self isHomeRooted not and: [ aHasIdCounter not]) ifTrue: [ ^nil].

	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aTypeHome := aModel resolveReferencedTypeName: self class homeCMGOTypeName moduleNames: self class homeCMGOModuleNames.
	aTypeHome isNil ifTrue: [ ^nil].

	aSpecializedHomeMetaInfo := aTypeHome specializedHomeMetaInfoForHomeElementsType: self.
	^aSpecializedHomeMetaInfo! !

!CODEType publicMethodsFor: 'TRF-initialization'!

initializeNewObjectFeatures: theNewObject

	| someFeatures |

	theNewObject isNil ifTrue: [ ^self].

	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^self].

	someFeatures do: [:aFeature |
		aFeature initializeNewObjectFeature: theNewObject
	].! !

!CODEType publicMethodsFor: 'TRF-initialization-connect'!

initialize: theNewObject afterConnectionTo: theObject

	| someFeatures  aHasBeenInitialization |

	theNewObject isNil ifTrue: [ ^false].
	theObject isNil ifTrue: [ ^false].

	(self isTypeOfObjectInstance: theNewObject) ifFalse: [ ^false].

	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^false].

	aHasBeenInitialization := false.

	[  | aReTryInitialization |
		aReTryInitialization := false.
		someFeatures do: [:aFeature |  | aThereWasInitialization |
			(aFeature isAttribute or: [ aFeature isAggregation or: [ 
				aFeature computationKind = self class computationKindAfterConnection]]) ifTrue: [ 
				aThereWasInitialization := aFeature initialize: theNewObject afterConnectionTo: theObject.
				aReTryInitialization := aReTryInitialization or: [ aThereWasInitialization].
				aHasBeenInitialization := aHasBeenInitialization or: [ aThereWasInitialization].
			]
		].
		aReTryInitialization
	] whileTrue.

	^aHasBeenInitialization! !

!CODEType publicMethodsFor: 'TRF-initialization-ids'!

initializeNewObjectIDAttributes: theNewObject inHome: theHomeCMGO

	| someEffectiveAttributes |

	theNewObject isNil ifTrue: [ ^nil].
	theHomeCMGO isNil ifTrue: [ ^nil].

	someEffectiveAttributes := self allEffectiveAttributes.
	(someEffectiveAttributes isNil or: [ someEffectiveAttributes isEmpty]) ifTrue: [ ^false].

	 someEffectiveAttributes do: [:anAttribute | anAttribute initializeNewObjectIDAttributes: theNewObject inHome: theHomeCMGO]! !

!CODEType publicMethodsFor: 'TRF-initialization-required'!

initializeNewObjectRequiredFeatures: theNewObject withRequiredFeaturesObject: theRequiredFeaturesObject

	| someFeatures |

	theNewObject isNil ifTrue: [ ^self].
	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^self].

	someFeatures do: [:aFeature |
		aFeature initializeNewObjectRequiredFeature: theNewObject 
			withRequiredFeaturesObject: theRequiredFeaturesObject
			prefixes: Array new
	].!

initializeNewObjectRequiredFeatures: theNewObject 
	withRequiredFeaturesObject: theRequiredFeaturesObject
	prefixes: thePrefixes

	| someFeatures |

	theNewObject isNil ifTrue: [ ^self].

	someFeatures := self allEffectiveStructuralFeatures.
	(someFeatures isNil or: [ someFeatures isEmpty]) ifTrue: [ ^self].

	someFeatures do: [:aFeature |

		aFeature initializeNewObjectRequiredFeature: theNewObject 
			withRequiredFeaturesObject: theRequiredFeaturesObject
			prefixes: thePrefixes
	].! !

!CODEType publicMethodsFor: 'TRF-object accessing'!

getObjectDomainValue: theObject
	| aMetaInfo aAttributeObjectDomain aDomain aRelationshipDomain |

	theObject isNil ifTrue: [ ^nil].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	self isDomainType ifTrue: [ ^theObject].

	aDomain := self isHomeType 
		ifFalse: [ 
			aAttributeObjectDomain := aMetaInfo attributeOrInheritedNamed:  self class objectDomainCMGOAttributeName. 
			aAttributeObjectDomain isNil ifTrue: [ ^nil].
			aAttributeObjectDomain getObjectFeatureValueTC: theObject
		]
		ifTrue: [ 
			aRelationshipDomain := aMetaInfo relationshipOrInheritedNamed:  self class domainCMGOHomeRelationshipName. 
			aRelationshipDomain isNil ifTrue: [ ^nil].
			aRelationshipDomain getObjectFeatureValueTC: theObject
		].
	^aDomain!

getObjectIDValue: theObject
	^self getObjectIDValueNoDefault: theObject!

getObjectIDValueNoDefault: theObject
	| aIDAttribute aIDValue |
	theObject isNil ifTrue: [ ^nil].

	aIDAttribute := self effectiveIDAttribute.
	aIDAttribute isNil ifTrue: [ ^nil].

	aIDValue := aIDAttribute getObjectFeatureValueTC: theObject.
	aIDValue isNil ifFalse: [ ^aIDValue].

	^nil!

getObjectNameValue: theObject
	| aNameAttribute aNameValue aNameString anEnumValue aNameType aNameRelationship aNonVirtualType aMetaInfo |
	theObject isNil ifTrue: [ ^nil].

	self isEnumeration  ifTrue: [ 
		(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].
		anEnumValue := theObject enumValue.
		anEnumValue isNil ifTrue: [ ^nil].
		aNameValue := anEnumValue nlsName.
		^aNameValue
	].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].
	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].
	
	aNameAttribute := aMetaInfo nameAttribute.
	aNameAttribute isNil  ifTrue: [
		aNameRelationship := self nameRelationship.
		^aNameRelationship isNil  
			ifTrue: [  theObject printString]
			ifFalse: [ 
				aNameValue := aNameRelationship getRelatedObjectNameValueNoDefault: theObject.
				aNameValue isNil ifTrue: [ theObject printString] ifFalse: [ aNameValue]
			]
		].

	aNameValue := aNameAttribute getObjectFeatureValueTC: theObject.

	aNameValue isNil ifTrue: [ ^theObject printString].

	aNameType := aNameAttribute valueType.
	aNameType isNil ifTrue: [ ^theObject printString].
	
	aNonVirtualType := aNameType nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^theObject printString].

	aNonVirtualType isEnumeration ifTrue: [ 
		^aNonVirtualType getObjectNameValue: aNameValue
	].

	aNameString := aNonVirtualType primitiveBroker nameStringFromObject: aNameValue.
	aNameString isNil ifTrue: [ ^theObject printString].

	^aNameString!

getObjectNameValueNoDefault: theObject
	| aNameAttribute aNameValue aNameString anEnumValue aNameType aNameRelationship |
	theObject isNil ifTrue: [ ^nil].

	self isEnumeration  ifTrue: [ 
		(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].
		anEnumValue := theObject enumValue.
		anEnumValue isNil ifTrue: [ ^nil].
		aNameValue := anEnumValue nlsName.
		^aNameValue
	].

	aNameAttribute := self nameAttribute.

	aNameValue := aNameAttribute isNil 
		ifTrue: [
			aNameRelationship := self nameRelationship.
			aNameRelationship isNil 
				ifTrue: [  nil]
				ifFalse: [ aNameRelationship getRelatedObjectNameValueNoDefault: theObject]
		]
		ifFalse: [ aNameAttribute getObjectFeatureValueTC: theObject].

	aNameValue isNil ifTrue: [ ^nil].

	aNameType := aNameAttribute valueType.
	aNameType isNil ifTrue: [ ^aNameValue asString].

	aNameString := aNameType primitiveBroker nameStringFromObject: aNameValue.
	aNameString isNil ifFalse: [ ^aNameString].

	^nil!

getTerminalObjectNameValue: theObject
	
	| anEnumValueAttribute |
	theObject isNil ifTrue: [ ^nil].

	self isPrimitive ifTrue: [ ^theObject printString].

	self isEnumeration ifFalse: [ ^self getObjectNameValue: theObject].

	anEnumValueAttribute := theObject enumValue.
	anEnumValueAttribute isNil ifTrue: [ ^nil].
	
	^anEnumValueAttribute getNLSTerminalName!

object: theObject setNameValue: theName

	| aNameAttribute |
	theObject isNil ifTrue: [ ^nil].

	aNameAttribute := self nameAttribute.
	aNameAttribute isNil ifTrue: [ ^nil].
	
	^aNameAttribute object: theObject setTC: theName!

printValue: theObject
	| aNameValue anEnumValue aNonVirtualType aPrintString |
	theObject isNil ifTrue: [ ^nil].

	self isEnumeration  ifTrue: [ 
		(self isTypeOfObjectInstance: theObject) ifFalse: [ ^nil].
		anEnumValue := theObject enumValue.
		anEnumValue isNil ifTrue: [ ^theObject printString].
		aNameValue := anEnumValue nlsName.
		^aNameValue
	].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^theObject printString].

	(self isPrimitive not and: [ aNonVirtualType isPrimitive not]) ifTrue: [ 
		^self getObjectNameValue: theObject
	].

	aPrintString := aNonVirtualType primitiveBroker nameStringFromObject: theObject.
	aPrintString isNil ifTrue: [ ^theObject printString].

	^aPrintString!

readValue: theObject
	| aValue aString |

	theObject isNil ifTrue: [ ^nil].

	aString := theObject asString.

	aValue := self createObjectFromLiteral: aString.
	^aValue! !

!CODEType publicMethodsFor: 'TRF-synthesis'!

inverseSynthetizedRelationships
	| someRelationships someInverseSynthetizedRelationships |
	someRelationships := self allEffectiveRelationships.
	someInverseSynthetizedRelationships := someRelationships select: [:aRelationship |  | anInverse |
		anInverse := aRelationship inverse.
		anInverse isNil not and: [ anInverse isSynthetizedRelationship]
	].
	^someInverseSynthetizedRelationships!

isSynthetized
	^self synthesisRelationship isNil not!

synthesisRelationship
	| someRelationships aSynthesisRelationship |
	someRelationships := self allEffectiveRelationships.
	aSynthesisRelationship := someRelationships detect: [:aRelationship | aRelationship isSynthesisRelationship] ifNone: [ nil].
	^aSynthesisRelationship!

synthetizedRelationships
	| someRelationships someSynthetizedRelationships |
	someRelationships := self allEffectiveRelationships.
	someSynthetizedRelationships := someRelationships select: [:aRelationship | aRelationship isSynthetizedRelationship].
	^someSynthetizedRelationships! !

!CODEType publicMethodsFor: 'TRF-type membership'!

candidateFactoryTypes

	| someTypes someScannedTypes |
	someTypes := IdentitySet new: 13.
	someScannedTypes := IdentitySet new: 13.
	self candidateFactoryTypesInto: someTypes alreadyScanned: someScannedTypes.
	^someTypes!

candidateFactoryTypesInto: theTypes alreadyScanned: theAlreadyScanned

	(theAlreadyScanned includes: self) ifTrue: [ ^self].

	theAlreadyScanned add: self.

	self isAbstract ifFalse:[ 
		theTypes add: self
	].

	self subTypesPrivate do: [:aSubType |	
		aSubType candidateFactoryTypesInto: theTypes alreadyScanned: theAlreadyScanned
	].!

initMemberInstanceTypesCache 


	memberInstanceTypesCache := self memberInstanceTypesNoCache!

isDomainOrHomeType

	^self isDomainType or: [ self isHomeType]!

isDomainType


	| aModel aTypeDomain |
	aModel := self model.
	aModel isNil ifTrue: [ ^false].

	aTypeDomain := aModel resolveReferencedTypeName: self class domainCMGOTypeName 
		moduleNames: self class domainCMGOModuleNames.
	(aTypeDomain isNil not and: [ aTypeDomain == self]) ifTrue: [ ^true].

	^self hasSuperType: aTypeDomain!

isHomeType


	| aModel aTypeHome |
	aModel := self model.
	aModel isNil ifTrue: [ ^false].

	aTypeHome := aModel resolveReferencedTypeName: self class homeCMGOTypeName 
		moduleNames: self class homeCMGOModuleNames.
	(aTypeHome isNil not and: [ aTypeHome == self]) ifTrue: [ ^true].

	^self hasSuperType: aTypeHome!

isNonEmptyPrimitiveInstance: theObject

	| aNonVirtualType |

	theObject isNil ifTrue: [ ^false].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^false].

	(self isPrimitive or: [ aNonVirtualType isPrimitive]) ifFalse: [ ^false].

	(theObject isKindOf: CMGenericObject) ifTrue: [ ^false].

	(self isTypeOfObjectInstance: theObject) ifFalse: [ ^false].

	^self primitiveBroker isNonEmpty: theObject!

isNonEmptyPrimitiveOrValidEnumerationInstance: theObject

	| aNonVirtualType |

	theObject isNil ifTrue: [ ^false].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^false].

	(self isPrimitive or: [ aNonVirtualType isPrimitive]) ifTrue: [ ^self isNonEmptyPrimitiveInstance: theObject].

	(self isEnumeration or: [ aNonVirtualType isEnumeration]) ifTrue: [ ^self isValidEnumerationInstance: theObject].
	^false!

isTypeOfObjectInstance: theObject

	| aType aNonVirtualType someMemberTypes aResult aReengineredClassName aReengineredClass |

	(theObject isKindOf: CMGenericObject) ifTrue: [ 
		aType := theObject metaInfo.
		aType isNil ifTrue: [ ^false].
		aType == self ifTrue: [ ^true].
		someMemberTypes := self memberInstanceTypes.
		(someMemberTypes isNil or: [ someMemberTypes isEmpty]) ifTrue: [ ^false].
		aResult := someMemberTypes includes: aType.
		^aResult
	].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^false].

	(self isPrimitive or: [ aNonVirtualType isPrimitive]) ifTrue: [ 
		^self primitiveBroker isTypeOf: theObject
	].
 
	aReengineredClassName := self reengineredClassName.
	(aReengineredClassName isNil not and: [ aReengineredClassName isEmpty not]) ifTrue: [
		aReengineredClass := Smalltalk at: aReengineredClassName ifAbsent: [ nil].
		aReengineredClass isNil ifFalse: [
			^theObject isKindOf: aReengineredClass
		]
	].

	^self primitiveBroker isNil not and: [ self primitiveBroker isTypeOf: theObject]!

isValidEnumerationInstance: theEnumerationInstance

	| anEnumValue someEnumAttributes aNonVirtualType aEnumerationMetaInfo aFoundEnumValue |

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue:  [ ^nil].

	aNonVirtualType isEnumeration ifFalse: [ ^nil].

	theEnumerationInstance isNil ifTrue: [ ^false].

	aEnumerationMetaInfo := theEnumerationInstance metaInfo.
	aEnumerationMetaInfo isNil ifTrue:  [ ^false].
	aEnumerationMetaInfo isEnumeration ifFalse:  [ ^false].

	anEnumValue := theEnumerationInstance enumValue.
	anEnumValue isNil ifTrue:  [ ^false].
	anEnumValue isAttribute ifFalse: [ ^false].
	anEnumValue isEnumerationValue ifFalse: [ ^false].

	someEnumAttributes := aEnumerationMetaInfo allEnumerationAttributes.
	(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^false].
	
	aFoundEnumValue := someEnumAttributes detect: [:anEnumAttr | anEnumAttr name = anEnumValue name]
		ifNone: [ nil].
	aFoundEnumValue isNil ifTrue: [ ^false].
	^true!

memberInstanceTypes 

	^self memberInstanceTypesNoCache!

memberInstanceTypesFromCache 


	memberInstanceTypesCache isNil ifTrue: [self initMemberInstanceTypesCache].
	^memberInstanceTypesCache!

memberInstanceTypesNoCache 

	| someMemberInstanceTypes |
	someMemberInstanceTypes := IdentitySet new: 13.

	someMemberInstanceTypes add: self.

	self subTypesPrivate do: [:aSubType |	
		aSubType allSubTypesInto: someMemberInstanceTypes
	].

	self superTypesPrivate do: [:aSuperType |	
		aSuperType allAbstractSuperTypesInto: someMemberInstanceTypes
	].

	self isVirtual ifTrue: [ 
		self superTypesPrivate do: [:aSuperType |	
			aSuperType allVirtualAndFirstNonVirtualSuperTypesInto: someMemberInstanceTypes
		].
	].

	^someMemberInstanceTypes!

xisTypeOfObjectInstance: theObject

	| aType aNonVirtualType |

	(theObject isKindOf: CMGenericObject) ifTrue: [ 
		aType := theObject metaInfo.
		aType isNil ifTrue: [ ^false].
		aType == self ifTrue: [ ^true].
		^self isVirtual 
			ifTrue: [(self superTypes detect: [:aSuperType | aSuperType isTypeOfObjectInstanceOfVirtualType: theObject] ifNone: [ nil]) notNil]
			ifFalse: [ (self subTypes detect: [:aSubType | aSubType isTypeOfObjectInstance: theObject] ifNone: [ nil]) notNil ]
	].

	aNonVirtualType := self nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^false].

	(self isPrimitive or: [ aNonVirtualType isPrimitive]) ifTrue: [ 
		^self primitiveBroker isTypeOf: theObject
	].

	^self primitiveBroker isNil not and: [ self primitiveBroker isTypeOf: theObject]!

xisTypeOfObjectInstanceOfVirtualType: theObject

	| aType |

	(theObject isKindOf: CMGenericObject) ifFalse: [ ^false].

	aType := theObject metaInfo.
	aType isNil ifTrue: [ ^false].
	aType == self ifTrue: [ ^true].
	
	^self isVirtual 
		ifTrue: [ (self superTypes detect: [:aSuperType | aSuperType isTypeOfObjectInstanceOfVirtualType: theObject] ifNone: [ nil]) notNil]
		ifFalse: [ 
			((self subTypes select: [ :aSubType | aSubType isVirtual not])
				detect: [:aSubType | aSubType isTypeOfObjectInstance: theObject] ifNone: [ nil]) notNil
		].! !

CMSystemPrimitiveBroker initializeAfterLoad!
CMTypeDependency initializeAfterLoad!
CMTypeObserver initializeAfterLoad!
CMTransaction initializeAfterLoad!
CMGenericObject initializeAfterLoad!
CMGenericLinkMaker initializeAfterLoad!
CMGenericChangeEventsMaker initializeAfterLoad!
CMGenericDeletionChangeEventsMaker initializeAfterLoad!
CMTransactionNester initializeAfterLoad!
CODE_META_TRF initializeAfterLoad!

CODE_META_TRF loaded!
