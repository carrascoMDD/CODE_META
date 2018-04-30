'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Translation in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Translation becomeDefault!

Object subclass: #CMTranslationUtility
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Translation becomeDefault!

SubApplication subclass: #CODE_META_Translation
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Translation becomeDefault!

!CMTranslationUtility class publicMethodsFor: 'examples'!

example01
	"CMTranslationUtility example01 browsePath"

	| aModel anApplicationTranslation |
	aModel := CODEElement newFromPersistenceAsCode: CMViewModel exampleCODEViewModelForCMViews.
	anApplicationTranslation := self buildApplicationTranslationFromModel: aModel.
	^anApplicationTranslation!

example02
	"CMTranslationUtility example02 browsePath"

	| aModel anApplicationTranslation |
	aModel := CODEElement newFromPersistenceAsCode: PROTOprENV13606MetaInfoHolder prENV13606Store.
	anApplicationTranslation := self buildApplicationTranslationFromModel: aModel.
	^anApplicationTranslation!

example03
	"CMTranslationUtility example03 "

	| aModel  aDefinitionsHolder anObjectInstance aMetaInfo anApplicationTranslation |

	anObjectInstance := CMGenericObject example02.

	aMetaInfo := anObjectInstance metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	anApplicationTranslation := self buildApplicationTranslationFromModel: aModel.

	aDefinitionsHolder := CMDefinitionsHolder fromModel: aModel.

	CODEMModelPathFinderGenericBrowser
		openForObject: 			anObjectInstance 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at:  METABrowser applicationTranslationParameterSymbol put: anApplicationTranslation;
				yourself)!

example04UpdateprENV13606Translation
	"CMTranslationUtility example04UpdateprENV13606Translation "

	| aModel  anApplicationTranslation |

	aModel := CODEElement newFromPersistenceAsCode: PROTOprENV13606MetaInfoHolder prENV13606Store.
	aModel isNil ifTrue: [ ^nil].

	anApplicationTranslation := TranslationModelBase newFromPersistenceAsCode: PROTOprENV13606TranslationHolder prEnv13606TranslationStore.
	anApplicationTranslation isNil ifTrue: [ ^nil].

	self buildApplicationTranslation: anApplicationTranslation fromModule: aModel.

	anApplicationTranslation browsePath!

example05
	"CMTranslationUtility example05 "

	| aModel  aDefinitionsHolder aMetaInfo anApplicationTranslation aTypeEHCRRoot aEHCRRoot |

	aModel := PROTOprENV13606MetaInfoHolder  currentModel. 
	aTypeEHCRRoot := aModel resolveOrNewReferencedTypeName: 'EHCRRootArchitecturalComponent' moduleNames: #('Core').
	aEHCRRoot := CMGenericObject newWithMetaInfo: aTypeEHCRRoot.

	aMetaInfo := aEHCRRoot metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	anApplicationTranslation := self buildApplicationTranslationFromModel: aModel.

	aDefinitionsHolder := CMDefinitionsHolder fromModel: aModel.

	CODEMModelPathFinderGenericBrowser
		openForObject: 			aEHCRRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at:  METABrowser applicationTranslationParameterSymbol put: anApplicationTranslation;
				yourself)!

example06
	"CMTranslationUtility example06 "

	| aModel  aDefinitionsHolder aMetaInfo anApplicationTranslation aTypeEHCRRoot aEHCRRoot aBrowserClass |

	aModel := PROTOprENV13606MetaInfoHolder  currentModel. 
	aTypeEHCRRoot := aModel resolveOrNewReferencedTypeName: 'EHCRRootArchitecturalComponent' moduleNames: #('Core').
	aEHCRRoot := CMGenericObject newWithMetaInfo: aTypeEHCRRoot.

	aMetaInfo := aEHCRRoot metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	anApplicationTranslation := PROTOprENV13606TranslationHolder  currentTranslation.

	aDefinitionsHolder := CMDefinitionsHolder fromModel: aModel.

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ CODEMModelPathFinderGenericBrowser].
	
	aBrowserClass 
		openForObject: 			aEHCRRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at:  METABrowser applicationTranslationParameterSymbol put: anApplicationTranslation;
				yourself)!

example07
	"CMTranslationUtility example07 "

	| aModel  aDefinitionsHolder aMetaInfo anApplicationTranslation aTypeEHCRRoot aEHCRRoot aBrowserClass |

	aModel := PROTOprENV13606ViewsHolder  currentModelStoreMethodSelector: #prENV13606FoldersManagementViewStore. 
	aTypeEHCRRoot := aModel resolveOrNewReferencedTypeName: 'EHCRRootArchitecturalComponent' moduleNames: #('Core').
	aEHCRRoot := CMGenericObject newWithMetaInfo: aTypeEHCRRoot.

	aMetaInfo := aEHCRRoot metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	anApplicationTranslation := PROTOprENV13606ViewsTranslationHolder  currentTranslationStoreMethodSelector: #prEnv13606FoldersManagementViewTranslationStore.

	aDefinitionsHolder := CMDefinitionsHolder fromModel: aModel.

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ CODEMModelPathFinderGenericBrowser].
	
	aBrowserClass 
		openForObject: 			aEHCRRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				at:  METABrowser applicationTranslationParameterSymbol put: anApplicationTranslation;
				yourself)!

example08
	"CMTranslationUtility example08 "

	| aModel  aDefinitionsHolder aMetaInfo aTypeEHCRRoot aEHCRRoot aBrowserClass |

	aModel := PROTOprENV13606ViewsHolder  currentModelStoreMethodSelector: #prENV13606FoldersManagementViewStore. 
	aTypeEHCRRoot := aModel resolveReferencedTypeName: 'EHCRRootArchitecturalComponent' moduleNames: #().
	aEHCRRoot := CMGenericObject newWithMetaInfo: aTypeEHCRRoot.

	aMetaInfo := aEHCRRoot metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := CMDefinitionsHolder fromModel: aModel.

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ CODEMModelPathFinderGenericBrowser].
	
	aBrowserClass 
		openForObject: 			aEHCRRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				yourself)!

example09
	"CMTranslationUtility example09 "

	| aModel  aDefinitionsHolder aMetaInfo aEHCRRoot aBrowserClass |


	aEHCRRoot := PROTOprENV13606InfoHolder  currentInfoStoreMethodSelector: #prENV13606InfoStore.

	aMetaInfo := aEHCRRoot metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := CMDefinitionsHolder fromModel: aModel.

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ CODEMModelPathFinderGenericBrowser].
	
	aBrowserClass 
		openForObject: 			aEHCRRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				yourself)! !

!CMTranslationUtility class publicMethodsFor: 'translation building'!

buildApplicationTranslation: theApplicationTranslation fromAttribute: theAttribute

	| anAttributeGroupName anAttributeItemName anAttributeItemNameOne aReverseLinkItemName |

	theApplicationTranslation isNil ifTrue: [ ^nil].
	theAttribute isNil ifTrue: [ ^nil].

	anAttributeGroupName := theAttribute nameNLSGroupName.
	anAttributeItemName := theAttribute nameNLSItemName.
	theApplicationTranslation getOrRecordNLSGroup: anAttributeGroupName item: anAttributeItemName  translation: theAttribute nameNLSItemInitialTranslation.

	theAttribute isMultiplicityMany ifTrue: [ 
		anAttributeItemNameOne := theAttribute attributeNameNLSItemNameOne.
		theApplicationTranslation getOrRecordNLSGroup: anAttributeGroupName item: anAttributeItemNameOne  translation: theAttribute attributeNameNLSItemTranslationOne.
	].

	aReverseLinkItemName := theAttribute prEnv13606DataLinkReverseLinkNameNLSItemName.
	(aReverseLinkItemName isNil or: [ aReverseLinkItemName isEmpty]) ifFalse: [ 
		theApplicationTranslation getOrRecordNLSGroup: anAttributeGroupName item: aReverseLinkItemName 
			 translation: theAttribute prEnv13606DataLinkReverseLinkNameNLSItemInitialTranslation
	]!

buildApplicationTranslation: theApplicationTranslation fromModule: theModule

	| aModuleGroupName someTypes someSubModules aModuleItemName |

	theApplicationTranslation isNil ifTrue: [ ^nil].
	theModule isNil ifTrue: [ ^nil].

	aModuleGroupName := theModule moduleNameNLSGroupName.
	aModuleItemName := theModule moduleNameNLSItemName.
	theApplicationTranslation getOrRecordNLSGroup: aModuleGroupName item: aModuleItemName translation: theModule moduleNameNLSItemInitialTranslation.
	
	someTypes := theModule types.
	(someTypes isNil not and: [ someTypes isEmpty not]) ifTrue: [ 
		someTypes do: [:aType |
			self buildApplicationTranslation: theApplicationTranslation fromType: aType.
		]
	].


	someSubModules := theModule subModules.
	(someSubModules isNil not and: [ someSubModules isEmpty not]) ifTrue: [ 
		someSubModules do: [:aSubModule |
			self buildApplicationTranslation: theApplicationTranslation fromModule: aSubModule
		]
	].!

buildApplicationTranslation: theApplicationTranslation fromRelationship: theRelationship

	| aRelationshipGroupName aRelationshipItemName aRelationshipItemNameOne |

	theApplicationTranslation isNil ifTrue: [ ^nil].
	theRelationship isNil ifTrue: [ ^nil].

	aRelationshipGroupName := theRelationship relationshipNameNLSGroupName.
	aRelationshipItemName := theRelationship relationshipNameNLSItemName.
	theApplicationTranslation getOrRecordNLSGroup: aRelationshipGroupName item: aRelationshipItemName  translation: theRelationship relationshipNameNLSItemTranslation.

	theRelationship isMultiplicityMany ifTrue: [ 
		aRelationshipItemNameOne := theRelationship relationshipNameNLSItemNameOne.
		theApplicationTranslation getOrRecordNLSGroup: aRelationshipGroupName item: aRelationshipItemNameOne  translation: theRelationship relationshipNameNLSItemTranslationOne.
	].!

buildApplicationTranslation: theApplicationTranslation fromType: theType

	| aTypeGroupName aTypeItemName someAttributes someRelationships aTypeGroup  someSuperTypes  |

	theApplicationTranslation isNil ifTrue: [ ^nil].
	theType isNil ifTrue: [ ^nil].

	aTypeGroupName := theType typeNameNLSGroupName.
	aTypeItemName := theType typeNameNLSItemName.
	theApplicationTranslation getOrRecordNLSGroup: aTypeGroupName item: aTypeItemName  translation: theType typeNameNLSItemTranslation.

	aTypeGroup := theApplicationTranslation getOrRecordNLSGroup: aTypeGroupName.
	aTypeGroup isNil ifFalse: [ 
		someSuperTypes := theType superTypes.
		(someSuperTypes isNil not and: [ someSuperTypes isEmpty not]) ifTrue: [ 
			someSuperTypes do: [:aSuperType |  | aSuperTypeGroupName aSuperTypeGroup |
 				aSuperTypeGroupName := aSuperType typeNameNLSGroupName.
				aSuperTypeGroup := theApplicationTranslation getOrRecordNLSGroup: aSuperTypeGroupName.
				aSuperTypeGroup isNil ifFalse: [ 
					aSuperTypeGroup subGroupTranslationsAdd: aTypeGroup
				]
		]
	].
			

	].

	someAttributes := theType attributes.
	(someAttributes isNil not and: [ someAttributes isEmpty not]) ifTrue: [ 
		someAttributes do: [:anAttribute |
			self buildApplicationTranslation: theApplicationTranslation fromAttribute: anAttribute.
		]
	].

	someRelationships := theType relationships.
	(someRelationships isNil not and: [ someRelationships isEmpty not]) ifTrue: [ 
		someRelationships do: [:aRelationship |
			self buildApplicationTranslation: theApplicationTranslation fromRelationship: aRelationship.
		]
	].!

buildApplicationTranslationFromModel: theModel

	| anApplicationTranslation anApplicationTranslationName |
	theModel isNil ifTrue: [ ^nil].
	
	anApplicationTranslationName := theModel nlsAppName.
	anApplicationTranslation := ApplicationTranslation new.
	anApplicationTranslation name: anApplicationTranslationName.

	self buildApplicationTranslation: anApplicationTranslation fromModule: theModel.

	^anApplicationTranslation! !

!CMTranslationUtility class publicMethodsFor: 'translations hacking'!

hackALittle

(GroupTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [ aGT name size >   'Module_' size and: [ 
		(aGT name copyFrom: 1 to: 'Module_' size) = 'Module_'
	]]
]) do: [:aGT |  | aGTN |
	aGTN := aGT name.
	aGT name: (aGTN copyFrom: 'Module_' size  + 1 to: aGTN size), '_Module'.
].

(GroupTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [ aGT name size >   'Type_' size and: [ 
		(aGT name copyFrom: 1 to: 'Type_' size) = 'Type_'
	]]
]) do: [:aGT |  | aGTN |
	aGTN := aGT name.
	aGT name: (aGTN copyFrom: 'Type_' size  + 1 to: aGTN size), '_Type'.
].

(ItemTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [ aGT name size >   'Module_' size and: [ 
		(aGT name copyFrom: 1 to: 'Module_' size) = 'Module_'
	]]
]) do: [:aGT |  | aGTN  aGTNwon|
	aGTN := aGT name.
	aGTNwon := (aGTN copyFrom: aGTN size - '_Name' size + 1 to: aGTN size) = '_Name' ifTrue: [aGTN copyFrom: 1 to: aGTN size - '_Name' size]  ifFalse: [aGTN].
	aGT name: (aGTNwon copyFrom: 'Module_' size  + 1 to: aGTNwon size), '_Module'.
].

(ItemTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [ aGT name size >   'Type_' size and: [ 
		(aGT name copyFrom: 1 to: 'Type_' size) = 'Type_'
	]]
]) do: [:aGT |  | aGTN  aGTNwon|
	aGTN := aGT name.
	aGTNwon := (aGTN copyFrom: aGTN size - '_Name' size + 1 to: aGTN size) = '_Name' ifTrue: [aGTN copyFrom: 1 to: aGTN size - '_Name' size]  ifFalse: [aGTN].
	aGT name: (aGTNwon copyFrom: 'Type_' size  + 1 to: aGTNwon size), '_Type'.
].

(ItemTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [ aGT name size >   'Attribute_' size and: [ 
		(aGT name copyFrom: 1 to: 'Attribute_' size) = 'Attribute_'
	]]
]) do: [:aGT |  | aGTN  aGTNwon|
	aGTN := aGT name.
	aGTNwon := (aGTN copyFrom: aGTN size - '_Name' size + 1 to: aGTN size) = '_Name' ifTrue: [aGTN copyFrom: 1 to: aGTN size - '_Name' size]  ifFalse: [aGTN].
	aGT name: (aGTNwon copyFrom: 'Attribute_' size  + 1 to: aGTNwon size), '_Attribute'.
].

(ItemTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [ aGT name size >   'Relationship_' size and: [ 
		(aGT name copyFrom: 1 to: 'Relationship_' size) = 'Relationship_'
	]]
]) do: [:aGT |  | aGTN  aGTNwon|
	aGTN := aGT name.
	aGTNwon := (aGTN copyFrom: aGTN size - '_Name' size + 1 to: aGTN size) = '_Name' ifTrue: [aGTN copyFrom: 1 to: aGTN size - '_Name' size]  ifFalse: [aGTN].
	aGT name: (aGTNwon copyFrom: 'Relationship_' size  + 1 to: aGTNwon size), '_Relationship'.
].


(ItemTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [(aGT name findString:  '_Name_'  startingAt: 1) > 0]
]) do: [:aGT |  | aGTN  aGTNwon  anIndex |
	aGTN := aGT name.
	anIndex := aGT name findString:  '_Name_'  startingAt: 1.
	aGTNwon := aGTN copyReplaceFrom: anIndex to: anIndex + '_Name_' size -1  with: '_'.
	aGT name: aGTNwon.
].


(ItemTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [(aGT name findString:  '_NameOne_Attribute'  startingAt: 1) > 0]
]) do: [:aGT |  | aGTN  aGTNwon  anIndex |
	aGTN := aGT name.
	anIndex := aGT name findString:  '_NameOne_Attribute'  startingAt: 1.
	aGTNwon := aGTN copyReplaceFrom: anIndex to: anIndex + '_NameOne_Attribute' size -1  with: '_Attribute_NameOne'.
	aGT name: aGTNwon.
].

(ItemTranslation allInstances select: [:aGT | 
	aGT name isNil not and: [(aGT name findString:  '_NameOne_Relationship'  startingAt: 1) > 0]
]) do: [:aGT |  | aGTN  aGTNwon  anIndex |
	aGTN := aGT name.
	anIndex := aGT name findString:  '_NameOne_Relationship'  startingAt: 1.
	aGTNwon := aGTN copyReplaceFrom: anIndex to: anIndex + '_NameOne_Relationship' size -1  with: '_Relationship_NameOne'.
	aGT name: aGTNwon.
].! !

!CODEArgument publicMethodsFor: 'nls'!

itemTranslations

	^self isMultiplicityMany
		ifTrue: [ super itemTranslations, super itemTranslationsOne]
		ifFalse: [  super itemTranslations]!

itemTranslationsLinkCreate

	super itemTranslationsLinkCreate.

	self isMultiplicityMany ifTrue: [ super itemTranslationsOneLinkCreate]!

nameNLSGroupName
	| aOperation |
	
	aOperation := self operation.
	aOperation isNil ifTrue: [ ^nil].
	^aOperation operationArgumentsNLSGroupName!

nameNLSItemName
	| anItemName |
	anItemName := self name, self class argumentNameNLSItemNamePostfix.
	^anItemName!

nameNLSItemNameOne

	^self isMultiplicityMany
		ifTrue: [ self nameNLSItemName, self class argumentNameNLSItemNameOnePostfix]
		ifFalse: [ '-- unused : multiplicity not many : numero maximo de elementos no plural --']!

nlsNameOne

	^self isMultiplicityMany
		ifTrue: [ super nlsNameOne]
		ifFalse: [ '-- unused : multiplicity not many : numero maximo de elementos no plural --']! !

!CODEAttribute publicMethodsFor: 'nls'!

itemTranslations

	^self isMultiplicityMany
		ifTrue: [ super itemTranslations, super itemTranslationsOne]
		ifFalse: [  super itemTranslations]!

itemTranslationsLinkCreate

	super itemTranslationsLinkCreate.

	self isMultiplicityMany ifTrue: [ super itemTranslationsOneLinkCreate]!

nameNLSGroupName
	| aType |
	
	aType := self type.
	aType isNil ifTrue: [ ^nil].
	^aType typeAttributesNLSGroupName!

nameNLSItemName
	| anItemName |
	anItemName := self name, self class attributeNameNLSItemNamePostfix.
	^anItemName!

nameNLSItemNameOne

	^self isMultiplicityMany
		ifTrue: [ self nameNLSItemName, self class attributeNameNLSItemNameOnePostfix]
		ifFalse: [ '-- unused : multiplicity not many : numero maximo de elementos no plural --']!

nlsNameOne

	^self isMultiplicityMany
		ifTrue: [ super nlsNameOne]
		ifFalse: [ '-- unused : multiplicity not many : numero maximo de elementos no plural --']!

prEnv13606DataLinkReverseLinkNameNLSItemInitialTranslation
	| aReverseLinkComment someSubStrings aReverseLinkName aReverseLinkText |

	aReverseLinkComment := self comments detect: [:aComment | aComment name = 'ReverseLink'] ifNone: [ nil].
	aReverseLinkComment isNil  ifTrue: [ ^nil].

  	aReverseLinkText :=  aReverseLinkComment text.
	(aReverseLinkText isNil or: [ aReverseLinkText isEmpty]) ifTrue: [ ^nil].

	someSubStrings :=aReverseLinkText asArrayOfSubstrings.
	someSubStrings isEmpty  ifTrue: [ ^nil].

	aReverseLinkName := someSubStrings first.
	^aReverseLinkName!

prEnv13606DataLinkReverseLinkNameNLSItemName
	| aReverseLinkComment someSubStrings aReverseLinkName aReverseLinkItemName aReverseLinkText |

	aReverseLinkComment := self comments detect: [:aComment | aComment name = 'ReverseLink'] ifNone: [ nil].
	aReverseLinkComment isNil  ifTrue: [ ^nil].

 	aReverseLinkText :=  aReverseLinkComment text.
	(aReverseLinkText isNil or: [ aReverseLinkText isEmpty]) ifTrue: [ ^nil].


	someSubStrings :=aReverseLinkText asArrayOfSubstrings.
	someSubStrings isEmpty  ifTrue: [ ^nil].

	aReverseLinkName := someSubStrings first.
	aReverseLinkItemName := self attributeNameNLSItemName, self class reverseLinkNameNLSItemNamePostfix,
		aReverseLinkName.
	^aReverseLinkItemName! !

!CODEElement class publicMethodsFor: 'nls'!

argumentNameNLSGroupNamePostfix
	^'_Argument' copy!

argumentNameNLSGroupNamePrefix
	^'Argument_' copy!

argumentNameNLSItemNameOnePostfix
	^'_NameOne' copy!

argumentNameNLSItemNamePostfix
	^'_Argument' copy!

argumentNameNLSItemNamePrefix
	^'Argument_' copy!

attributeNameNLSGroupNamePostfix
	^'_Attribute' copy!

attributeNameNLSGroupNamePrefix
	^'Attribute_' copy!

attributeNameNLSItemNameOnePostfix
	^'_NameOne' copy!

attributeNameNLSItemNamePostfix
	^'_Attribute' copy!

attributeNameNLSItemNamePrefix
	^'Attribute_' copy!

defaultNLSApp
	^'CODE' copy!

elementKindsNLSGroup
	^'ElementKinds' copy!

elementMapNameNLSGroupNamePostfix
	^'_ElementMap' copy!

elementMapNameNLSItemNamePostfix
	^'_ElementMap' copy!

mapsFolderNameNLSGroupNamePostfix
	^'_MapsFolder' copy!

mapsFolderNameNLSItemNamePostfix
	^'_MapsFolder' copy!

modelNameNLSGroupNamePostfix
	^'_Model' copy!

modelNameNLSGroupNamePrefix
	^'Model_' copy!

modelNameNLSItemNamePostfix
	^'_Model' copy!

modelNameNLSItemNamePrefix
	^'Model_' copy!

moduleNameNLSGroupNamePostfix
	^'_Module' copy!

moduleNameNLSGroupNamePrefix
	^'Module_' copy!

moduleNameNLSItemNamePostfix
	^'_Module' copy!

moduleNameNLSItemNamePrefix
	^'Module_' copy!

nlsApplicationTranslationNamePrefix
	^'Translation_' copy!

nlsAppNamePostfix
	^'_ApplicationTranslation' copy!

operationNameNLSGroupNamePostfix
	^'_Operation' copy!

operationNameNLSGroupNamePrefix
	^'Operation_' copy!

operationNameNLSItemNameOnePostfix
	^'_NameOne' copy!

operationNameNLSItemNamePostfix
	^'_Operation' copy!

operationNameNLSItemNamePrefix
	^'Operation_' copy!

relationshipNameNLSGroupNamePostfix
	^'_Relationship' copy!

relationshipNameNLSGroupNamePrefix
	^'Relationship_' copy!

relationshipNameNLSItemNameOnePostfix
	^'_NameOne' copy!

relationshipNameNLSItemNamePostfix
	^'_Relationship' copy!

relationshipNameNLSItemNamePrefix
	^'Relationship_' copy!

reverseLinkNameNLSItemNamePostfix
	^'_ReverseLink_' copy!

typeNameNLSGroupNamePostfix
	^'_Type' copy!

typeNameNLSGroupNamePrefix
	^'Type_' copy!

typeNameNLSItemNamePostfix
	^'_Type' copy!

typeNameNLSItemNamePrefix
	^'Type_' copy! !

!CODEElement class publicMethodsFor: 'preferences'!

preferredTranslationsHomeClass
	^TranslationsHome! !

!CODEElement publicMethodsFor: 'nls'!

itemTranslations

	| aNLSSolver someItemTranslations aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	someItemTranslations := OrderedCollection new: 2.

	aNLSItem := aNLSSolver nlsLocalResolverItemAppNoDefault: self nlsAppName group: self nameNLSGroupName item: self nameNLSItemName.
	aNLSItem isNil ifFalse: [ someItemTranslations add:  aNLSItem].

	^someItemTranslations!

itemTranslationsLinkCreate

	| aNLSSolver aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	aNLSItem := aNLSSolver nlsLocalResolverItemOrCreateApp: self nlsAppName group: self nameNLSGroupName
		item: self nameNLSItemName translation: self nameNLSItemInitialTranslation.

	self changed: #itemTranslations.
	^aNLSItem!

itemTranslationsOne

	| aNLSSolver someItemTranslations aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	someItemTranslations := OrderedCollection new: 1.

	aNLSItem := aNLSSolver nlsResolverItemApp: self nlsAppName group: self nameNLSGroupName item: self nameNLSItemNameOne.
	aNLSItem isNil ifFalse: [ someItemTranslations add:  aNLSItem].

	^someItemTranslations!

itemTranslationsOneLinkCreate

	| aNLSSolver aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	aNLSItem := aNLSSolver nlsLocalResolverItemOrCreateApp: self nlsAppName group: self nameNLSGroupName
		item: self nameNLSItemNameOne translation: self nameNLSItemOneInitialTranslation.

	self changed: #itemTranslations.
	^aNLSItem!

nameNLSGroupName
	^nil!

nameNLSItemInitialTranslation
	^self name!

nameNLSItemName
	^nil!

nameNLSItemOneInitialTranslation
	^self name!

nlsAppName
	| aModel |
	
	aModel := self model.
	aModel isNil ifTrue: [ ^nil].
	^aModel nlsAppName!

nlsKind
	
	| aNLSKind aNLSSolver |

	aNLSSolver := self nlsSolver.	
	aNLSSolver isNil ifTrue: [ ^self kind].

	aNLSKind := aNLSSolver nlsAppNoDefault: self class defaultNLSApp group: self class elementKindsNLSGroup item: self kind asString.
	(aNLSKind isNil or: [ aNLSKind isEmpty]) ifTrue: [ ^self kind].

	^aNLSKind!

nlsKindNoDefault
	
	| aNLSKind aNLSSolver |

	aNLSSolver := self nlsSolver.	
	aNLSSolver isNil ifTrue: [ ^nil].

	aNLSKind := aNLSSolver nlsAppNoDefault: self class defaultNLSApp group: self class elementKindsNLSGroup item: self kind asString.
	^aNLSKind!

nlsName
	
	| aNLSName aNLSSolver |

	aNLSSolver := self nlsSolver.	
	aNLSSolver isNil ifTrue: [ ^self nameNLSItemInitialTranslation].

	aNLSName := aNLSSolver nlsAppNoDefault: self nlsAppName group: self nameNLSGroupName item: self nameNLSItemName.
	(aNLSName isNil or: [ aNLSName isEmpty]) ifTrue: [ ^self name].

	^aNLSName!

nlsNameNoDefault
	
	| aNLSName aNLSSolver |

	aNLSSolver := self nlsSolver.	
	aNLSSolver isNil ifTrue: [ ^nil].

	aNLSName := aNLSSolver nlsAppNoDefault: self nlsAppName group: self nameNLSGroupName item: self nameNLSItemName.
	^aNLSName!

nlsNameOne
	
	| aNLSName aNLSSolver |

	aNLSSolver := self nlsSolver.	
	aNLSSolver isNil ifTrue: [ ^self nameNLSItemInitialTranslation].

	aNLSName := aNLSSolver nlsApp: self nlsAppName group: self nameNLSGroupName item: self nameNLSItemNameOne.
	(aNLSName isNil or: [ aNLSName isEmpty]) ifTrue: [ ^self nameNLSItemOneInitialTranslation].

	^aNLSName!

nlsSolver
	| aModel aNLSSolver |
	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	aNLSSolver := aModel nlsSolver.
	^aNLSSolver!

resolverItemTranslations

	| aNLSSolver someItemTranslations aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	someItemTranslations := OrderedCollection new: 2.

	aNLSItem := aNLSSolver nlsResolverItemApp: self nlsAppName group: self nameNLSGroupName item: self nameNLSItemName.
	aNLSItem isNil ifFalse: [ someItemTranslations add:  aNLSItem].

	^someItemTranslations! !

!CODEElement publicMethodsFor: 'preferences'!

preferredTranslationsHomeClass
	^self class preferredTranslationsHomeClass! !

!CODEModel publicMethodsFor: 'accessing'!

defaultApplicationTranslationStoreClassName
	^defaultApplicationTranslationStoreClassName!

defaultApplicationTranslationStoreClassName: elValor
	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ 
					unosStrings size > 1
						ifTrue: [ unosStrings first asSymbol]
						ifFalse: [ elValor asSymbol]
				]
		].
	
	unValor = defaultApplicationTranslationStoreClassName  ifTrue: [ ^self].

	defaultApplicationTranslationStoreClassName := unValor.
	self markDirty.

	self changed: #defaultApplicationTranslationStoreClassName!

defaultApplicationTranslationStoreMethodSelector
	^defaultApplicationTranslationStoreMethodSelector!

defaultApplicationTranslationStoreMethodSelector: elValor
	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ 
					unosStrings size > 1
						ifTrue: [ unosStrings first asSymbol]
						ifFalse: [ elValor asSymbol]
				]
		].
	
	unValor = defaultApplicationTranslationStoreMethodSelector  ifTrue: [ ^self].

	defaultApplicationTranslationStoreMethodSelector := unValor.
	self markDirty.

	self changed: #defaultApplicationTranslationStoreMethodSelector! !

!CODEModel publicMethodsFor: 'nls'!

itemTranslations

	| aNLSSolver aNLSItemModelName someItemTranslations |

	someItemTranslations := super itemTranslations.

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^someItemTranslations ].

	aNLSItemModelName := aNLSSolver nlsResolverItemApp: self nlsAppName group: self modelNameNLSGroupName 
		item: self modelNameNLSItemName.
	aNLSItemModelName isNil ifFalse: [ someItemTranslations add:  aNLSItemModelName].

	^someItemTranslations!

itemTranslationsLinkCreate

	| aNLSSolver aNLSItemModelName |

	super itemTranslationsLinkCreate.

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ aNLSSolver := TranslationsHome].

	aNLSItemModelName := aNLSSolver nlsLocalResolverItemOrCreateApp: self nlsAppName group: self modelNameNLSGroupName 
		item: self modelNameNLSItemName translation: self modelNameNLSItemInitialTranslation.

	self changed: #itemTranslations.
	^aNLSItemModelName!

modelNameNLSGroupName
	| aGroupName |
	aGroupName := self name, self class modelNameNLSGroupNamePostfix.
	^aGroupName!

modelNameNLSItemInitialTranslation
	^self name!

modelNameNLSItemName
	| anItemName |
	anItemName := self name, self class modelNameNLSItemNamePostfix.
	^anItemName!

nlsAppName
	^self name, self class nlsAppNamePostfix!

nlsSolver
	
	| aClassName aMethodSelector aClass aResult aKind anElement |

	aClassName := self defaultApplicationTranslationStoreClassName.
	aClassName isNil ifTrue: [ ^nil].

	aMethodSelector := self defaultApplicationTranslationStoreMethodSelector.
	aMethodSelector isNil ifTrue: [ ^nil].
	
	aClass := Smalltalk at: aClassName asSymbol ifAbsent: [ nil].
	aClass isNil ifTrue: [ ^nil].

	aResult := Object messageNotUnderstoodSignal 
		handle: [:anEx | anEx returnWith: nil]
		do:  [ aClass perform: #currentTranslationStoreMethodSelector: with: aMethodSelector].

	aResult isNil  ifTrue: [  aResult := aClass perform: aMethodSelector].
	
	aResult isNil  ifTrue: [ ^nil].

	((aResult isKindOf: ApplicationTranslation) or: [ 
		(aResult isKindOf: TranslationCatalog) or: [ aResult == nil]]) ifTrue: [ ^aResult].

	((aResult isKindOf: Array) and: [aResult isEmpty not]) ifFalse: [  ^nil].

	aKind := aResult first.
	(aKind isNil or: [ aKind = TranslationCatalog kind  or:  [ aKind = ApplicationTranslation kind ]]) ifFalse: [ ^nil].

	anElement := TranslationModelBase newFromPersistenceAsCode: aResult .
	anElement isNil ifTrue: [ ^nil].
	((anElement isKindOf: ApplicationTranslation) or: [ 
		(anElement isKindOf: TranslationCatalog) or: [ anElement == nil]]) ifFalse: [  ^nil].

	^anElement! !

!CODEModule publicMethodsFor: 'nls'!

nameNLSGroupName
	| aGroupName |
	aGroupName := self fullyQualifiedName, self class moduleNameNLSGroupNamePostfix.
	^aGroupName!

nameNLSItemName
	| aItemName |
	aItemName := self name, self class moduleNameNLSItemNamePostfix.
	^aItemName! !

!CODEOperation publicMethodsFor: 'nls'!

itemTranslations

	^super itemTranslations!

itemTranslationsLinkCreate

	super itemTranslationsLinkCreate.!

nameNLSGroupName
	| aType |
	
	aType := self type.
	aType isNil ifTrue: [ ^nil].
	^aType typeOperationsNLSGroupName!

nameNLSItemName
	| anItemName |
	anItemName := self name, self class operationNameNLSItemNamePostfix.
	^anItemName!

operationArgumentsNLSGroupName
	^self nameNLSGroupName! !

!CODERelationship publicMethodsFor: 'nls'!

itemTranslations

	^self isMultiplicityMany
		ifTrue: [ super itemTranslations, super itemTranslationsOne]
		ifFalse: [  super itemTranslations]!

itemTranslationsLinkCreate

	super itemTranslationsLinkCreate.

	self isMultiplicityMany ifTrue: [ super itemTranslationsOneLinkCreate]!

nameNLSGroupName
	| aType |
	
	aType := self type.
	aType isNil ifTrue: [ ^nil].
	^aType typeRelationshipsNLSGroupName!

nameNLSItemName
	| anItemName |
	anItemName := self name, self class relationshipNameNLSItemNamePostfix.
	^anItemName!

nameNLSItemNameOne

	^self isMultiplicityMany
		ifTrue: [ self nameNLSItemName, self class relationshipNameNLSItemNameOnePostfix]
		ifFalse: [ '-- unused : multiplicity not many : numero maximo de elementos no plural --']!

nlsNameOne

	^self isMultiplicityMany
		ifTrue: [ super nlsNameOne]
		ifFalse: [ '-- unused : multiplicity not many : numero maximo de elementos no plural --']! !

!CODEType publicMethodsFor: 'nls'!

nameNLSGroupName
	| aGroupName |
	aGroupName := self fullyQualifiedName, self class typeNameNLSGroupNamePostfix .
	^aGroupName!

nameNLSItemName
	| anItemName |
	anItemName := self name, self class typeNameNLSItemNamePostfix.
	^anItemName!

nlsClassLabel
	
	| aClassLabel aNLSKind aNLSName |

	aNLSKind := self nlsKind.

	aNLSName := self nlsName.

	aClassLabel := aNLSKind,  ' ' , aNLSName.
	^aClassLabel!

typeAttributesNLSGroupName
	^self nameNLSGroupName!

typeOperationsNLSGroupName
	^self nameNLSGroupName!

typeRelationshipsNLSGroupName
	^self nameNLSGroupName! !

CMTranslationUtility initializeAfterLoad!
CODE_META_Translation initializeAfterLoad!

CODE_META_Translation loaded!
