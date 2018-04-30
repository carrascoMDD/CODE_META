'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Support in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Support becomeDefault!

METADefinitionsHolder subclass: #CMDefinitionsHolder
	instanceVariableNames: 'model '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Support becomeDefault!

SubApplication subclass: #CODE_META_Support
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Support becomeDefault!

!CMDefinitionsHolder class publicMethodsFor: 'constants'!

generalPerspectiveName
	^'General' copy! !

!CMDefinitionsHolder class publicMethodsFor: 'examples'!

example01
	"CMDefinitionsHolder example01"

	| aModel  aDefinitionsHolder |

	aModel := CODEElement newFromPersistenceAsCode: CMViewModel exampleCODEViewModelForCMViews.

	aDefinitionsHolder := self fromModel: aModel.
	^aDefinitionsHolder!

example02
	"CMDefinitionsHolder example02"

	| aModel  aDefinitionsHolder anObjectInstance aMetaInfo |

	anObjectInstance := CMGenericObject example01.

	aMetaInfo := anObjectInstance metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := self fromModel: aModel.

	CODEMModelPathFinderGenericBrowser
		openForObject: 			anObjectInstance 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				yourself)!

example03
	"CMDefinitionsHolder example03"

	| aModel  aDefinitionsHolder anObjectInstance aMetaInfo |

	anObjectInstance := CMGenericObject example02.

	aMetaInfo := anObjectInstance metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aDefinitionsHolder := self fromModel: aModel.

	CODEMModelPathFinderGenericBrowser
		openForObject: 			anObjectInstance 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;
				yourself)! !

!CMDefinitionsHolder class publicMethodsFor: 'instance creation'!

fromModel: theModel

	| someViews somePerspectives aDefinitionsHolder somePaths someVPP |

	theModel isNil ifTrue: [ ^nil].

	someVPP := CODEMModelDefinitionsHolder viewsPerspectivesPathsForModelEditor.

	someViews 			:= (someVPP at: 1) copy.
	somePerspectives	:= (someVPP at: 2) copy.
	somePaths 			:= (someVPP at: 3) copy.

	self buildViews: someViews perspectives: somePerspectives paths: somePaths fromModel: theModel.

	aDefinitionsHolder := self 
		views: 							someViews
		applicationModels: 				nil
		listMenus:						nil
		styleModifiers:					nil
		perspectives:					somePerspectives
		perspectivesApplicationModels:	nil
		paths:								somePaths
		filters:								nil.

	aDefinitionsHolder model: theModel.

	^aDefinitionsHolder! !

!CMDefinitionsHolder class publicMethodsFor: 'preferences'!

preferredClassChildSpecClass
	^METAPreferences preferredClassChildSpecClass!

preferredCMClassChildSpecClass
	

	^self preferredPreferencesClass preferredCMClassChildSpecClass!

preferredCMCollectionChildSpecClass
	

	^self preferredPreferencesClass preferredCMCollectionChildSpecClass!

preferredCMOperationVoidNoArgsChildSpecClass
	

	^self preferredPreferencesClass preferredCMOperationVoidNoArgsChildSpecClass!

preferredCMOrderedCollectionChildSpecClass
	

	^self preferredPreferencesClass preferredCMOrderedCollectionChildSpecClass!

preferredCMPerspectiveSpecClass
	

	^self preferredPreferencesClass preferredCMPerspectiveSpecClass!

preferredCMTerminalChildSpecClass
	

	^self preferredPreferencesClass preferredCMTerminalChildSpecClass!

preferredCMTerminalCollectionChildSpecClass
	

	^self preferredPreferencesClass preferredCMTerminalCollectionChildSpecClass!

preferredCMTerminalOrderedCollectionChildSpecClass
	

	^self preferredPreferencesClass preferredCMTerminalOrderedCollectionChildSpecClass!

preferredCMTextChildSpecClass
	

	^self preferredPreferencesClass preferredCMTextChildSpecClass!

preferredPreferencesClass

	^CMPreferences! !

!CMDefinitionsHolder class publicMethodsFor: 'preferences-refin'!

preferredApplicationBrowserClass
	

	^self preferredPreferencesClass preferredApplicationBrowserClass!

preferredPathFinderApplicationBrowserClass
	^self preferredPreferencesClass preferredPathFinderApplicationBrowserClass! !

!CMDefinitionsHolder class publicMethodsFor: 'specs building'!

buildClassSpecFromAttribute: theAttribute

	| aSpecClass aValueType aSpec aNonVirtualType aIsCODEElement aObjectClassName aValueTypeSymbol anIsStatic aNameAttribute aDisplaySelector |
	theAttribute isNil ifTrue: [ ^nil].

	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].
	aValueTypeSymbol := aValueType name asSymbol.

	aIsCODEElement := aValueTypeSymbol = CODEType name or: [ 
		aValueTypeSymbol = CODEModel name or: [ 
		aValueTypeSymbol = CODEAttribute name or: [ 
		aValueTypeSymbol = CODERelationship name]]].
	
	(aValueType isPrimitive or: [ aValueType isEnumeration]) ifTrue:[ ^nil].
	
	aNonVirtualType := aValueType nonVirtualType.
	(aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]) ifTrue:[ ^nil].


	aSpecClass := self preferredCMClassChildSpecClass.

	aObjectClassName := aIsCODEElement
		ifFalse: [ #CMGenericObject] 
		ifTrue: [   aValueType name asSymbol].

	anIsStatic :=  (self canChangeFeature: theAttribute)  not or: [  
		theAttribute computationKind = theAttribute class computationKindAlways or: [ 
		theAttribute computationKind = theAttribute class computationKindInitializedInConstructor]].

	aNameAttribute := aValueType nameAttribute.
	aDisplaySelector := aNameAttribute isNil ifTrue: [nil] ifFalse: [ aNameAttribute name].

	aSpec := aSpecClass new
		name: theAttribute name ;
		basicSelector: theAttribute name asSymbol;
		type: #Object;
		displayValue: aIsCODEElement not;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theAttribute name  , ' of a ', theAttribute type name, ' (a ', aValueType name, ')';
		displaySelector: aDisplaySelector;
		objectClassName: aObjectClassName;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		showInEditor: aIsCODEElement not;
		menuSelector: nil;
		metaInfo: theAttribute;
		nlsGroup: theAttribute nameNLSGroupName;
		nlsItem: theAttribute nameNLSItemName;
		yourself.


	^aSpec!

buildClassSpecFromRelationship: theRelationship

	| aSpecClass aSpec aRelatedType anIsStatic aNonVirtualType aNameAttribute aDisplaySelector |
	theRelationship isNil ifTrue: [ ^nil].

	aRelatedType := theRelationship relatedType.
	aRelatedType isNil ifTrue: [ ^nil].

	(aRelatedType isPrimitive or: [ aRelatedType isEnumeration]) ifTrue:[ ^nil].
	
	aNonVirtualType := aRelatedType nonVirtualType.
	(aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]) ifTrue:[ ^nil].

	aSpecClass := self preferredCMClassChildSpecClass.
	anIsStatic := theRelationship isAggregated or: [
		 (self canChangeFeature: theRelationship) not or: [ 
			theRelationship computationKind = theRelationship class computationKindAlways]].

	aNameAttribute := aRelatedType nameAttribute.
	aDisplaySelector := aNameAttribute isNil ifTrue: [nil] ifFalse: [ aNameAttribute name].

	aSpec := aSpecClass new
		name: theRelationship name ;
		basicSelector: theRelationship name asSymbol;
		type: #Object;
		displayValue: true;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theRelationship name  , ' of a ', theRelationship type name, ' (a ', aRelatedType name, ')';
		displaySelector: aDisplaySelector;
		objectClassName: #CMGenericObject;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		showInEditor: true;
		menuSelector: nil;
		metaInfo:   theRelationship;
		nlsGroup: theRelationship nameNLSGroupName;
		nlsItem: theRelationship nameNLSItemName;
		yourself.

	^aSpec!

buildCollectionSpecFromAttribute: theAttribute

	| aSpecClass aSpec aNameAttribute aSortSelector anIsStatic aValueType aDisplayName aNonVirtualType |

	theAttribute isNil ifTrue: [ ^nil].

	theAttribute isMultiplicityMany  ifFalse: [ ^nil].
	theAttribute isOrdered  ifTrue: [ ^nil].


	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].

	(aValueType isPrimitive or: [ aValueType isEnumeration]) ifTrue:[ ^nil].
	aNonVirtualType := aValueType nonVirtualType.
	(aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]) ifTrue:[ ^nil].


	aNameAttribute := aValueType nameAttribute.
	aDisplayName := aNameAttribute isNil ifTrue: [ #name] ifFalse: [ aNameAttribute name].
	
	aSortSelector := theAttribute orderConstraint asSymbol.
	 

	aSortSelector := (aValueType hasOrInheritsFeatureNamed: aSortSelector asString) 
		ifFalse: [ 
			aNameAttribute isNil 
				ifFalse: [ aNameAttribute name asSymbol] 
				ifTrue: [ #printString]
		]
		ifTrue: [ aSortSelector].

	aSpecClass := self preferredCMCollectionChildSpecClass.

	anIsStatic :=  (self canChangeFeature: theAttribute) not or: [ 
		theAttribute computationKind = theAttribute class computationKindAlways ].


	aSpec := aSpecClass new
		name: theAttribute name ;
		basicSelector: theAttribute name asSymbol;
		type: #Object;
		displayValue: true;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theAttribute name  , ' of a ', theAttribute type name, ' (a ', aValueType name, ')';
		displaySelector: aDisplayName;
		canShowInTree: true;
		componentsClassName: #CMGenericObject;
		sortSelector: aSortSelector;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		autoFilter: true;
		metaInfo: theAttribute;
		nlsGroup: theAttribute nameNLSGroupName;
		nlsItem: theAttribute nameNLSItemName;
		yourself.

	^aSpec!

buildCollectionSpecFromRelationship: theRelationship

	| aSpecClass aSpec aRelatedType aNameAttribute aSortSelector anIsStatic aDisplayName aNonVirtualType |

	theRelationship isNil ifTrue: [ ^nil].

	theRelationship isMultiplicityMany  ifFalse: [ ^nil].
	theRelationship isOrdered  ifTrue: [ ^nil].


	aRelatedType := theRelationship relatedType.
	aRelatedType isNil ifTrue: [ ^nil].

	(aRelatedType isPrimitive or: [ aRelatedType isEnumeration]) ifTrue:[ ^nil].
	aNonVirtualType := aRelatedType nonVirtualType.
	(aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]) ifTrue:[ ^nil].

	aNameAttribute := aRelatedType nameAttribute.
	aDisplayName := aNameAttribute isNil ifTrue: [ #name] ifFalse: [ aNameAttribute name].
	
	aSortSelector := theRelationship orderConstraint asSymbol.
	 

	aSortSelector := (aRelatedType hasOrInheritsFeatureNamed: aSortSelector asString) 
		ifFalse: [ 
			aNameAttribute isNil 
				ifFalse: [ aNameAttribute name asSymbol] 
				ifTrue: [ #printString]
		]
		ifTrue: [ aSortSelector].


	aSpecClass := self preferredCMCollectionChildSpecClass.

	anIsStatic := theRelationship isAggregated or: [
		(self canChangeFeature: theRelationship) not or: [ 
			theRelationship computationKind = theRelationship class computationKindAlways ]].


	aSpec := aSpecClass new
		name: theRelationship name ;
		basicSelector: theRelationship name asSymbol;
		type: #Object;
		displayValue: true;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theRelationship name  , ' of a ', theRelationship type name, ' (a ', aRelatedType name, ')';
		displaySelector: aDisplayName;
		canShowInTree: true;
		componentsClassName: #CMGenericObject;
		sortSelector: aSortSelector;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		autoFilter: true;
		metaInfo: theRelationship;
		nlsGroup: theRelationship nameNLSGroupName;
		nlsItem: theRelationship nameNLSItemName;

		yourself.

	^aSpec!

buildMetaInfoSpec

	| aSpecClass aSpec |

	aSpecClass := self preferredClassChildSpecClass.

	aSpec := aSpecClass new
		name: 'MetaInfo';
		basicSelector: #metaInfo;
		type: #Object;
		displayValue: true;
		isChildren: true;
		isStatic: true;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: 'MetaInfo';
		displaySelector: nil;
		objectClassName: #CODEType;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		showInEditor: true;
		menuSelector: nil;
		yourself.

	^aSpec!

buildOrderedCollectionSpecFromAttribute: theAttribute

	| aSpecClass aSpec anIsStatic aValueType aNameAttribute aDisplayName |

	theAttribute isNil ifTrue: [ ^nil].

	theAttribute isMultiplicityMany  ifFalse: [ ^nil].
	theAttribute isOrdered  ifFalse: [ ^nil].

	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].

	(aValueType isPrimitive or: [ aValueType isEnumeration]) ifTrue:[ ^nil].

	aSpecClass := self preferredCMOrderedCollectionChildSpecClass.

	aNameAttribute := aValueType nameAttribute.
	aDisplayName := aNameAttribute isNil ifTrue: [ #name] ifFalse: [ aNameAttribute name].

	anIsStatic :=  (self canChangeFeature: theAttribute) not or: [ 
		theAttribute computationKind = theAttribute class computationKindAlways ].

	aSpec := aSpecClass new
		name: theAttribute name ;
		basicSelector: theAttribute name asSymbol;
		type: #Object;
		displayValue: true;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theAttribute name  , ' of a ', theAttribute type name, ' (a ', aValueType name, ')';
		displaySelector: aDisplayName;
		sortSelector:     	theAttribute orderConstraint asSymbol;
		canShowInTree: true;
		componentsClassName: #CMGenericObject;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		autoFilter: true;
		metaInfo: theAttribute;
		nlsGroup: theAttribute nameNLSGroupName;
		nlsItem: theAttribute nameNLSItemName;
		yourself.

	^aSpec!

buildOrderedCollectionSpecFromRelationship: theRelationship

	| aSpecClass aSpec aRelatedType anIsStatic aNameAttribute aDisplayName aNonVirtualType |

	theRelationship isNil ifTrue: [ ^nil].

	theRelationship isMultiplicityMany  ifFalse: [ ^nil].
	theRelationship isOrdered  ifFalse: [ ^nil].

	aRelatedType := theRelationship relatedType.
	aRelatedType isNil ifTrue: [ ^nil].

	(aRelatedType isPrimitive or: [ aRelatedType isEnumeration]) ifTrue:[ ^nil].
	aNonVirtualType := aRelatedType nonVirtualType.
	(aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]) ifTrue:[ ^nil].

	aSpecClass := self preferredCMOrderedCollectionChildSpecClass.

	aNameAttribute := aRelatedType nameAttribute.
	aDisplayName := aNameAttribute isNil ifTrue: [ #name] ifFalse: [ aNameAttribute name].

	anIsStatic := theRelationship isAggregated or: [
		(self canChangeFeature: theRelationship) not or: [ 
			theRelationship computationKind = theRelationship class computationKindAlways ]].

	aSpec := aSpecClass new
		name: theRelationship name ;
		basicSelector: theRelationship name asSymbol;
		type: #Object;
		displayValue: true;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theRelationship name  , ' of a ', theRelationship type name, ' (a ', aRelatedType name, ')';
		displaySelector: aDisplayName;
		sortSelector:  nil;
		canShowInTree: true;
		componentsClassName: #CMGenericObject;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		autoFilter: true;
		metaInfo: theRelationship;
		nlsGroup: theRelationship nameNLSGroupName;
		nlsItem: theRelationship nameNLSItemName;
		yourself.

	^aSpec!

buildPathsFromSpecs: theAllSpecs 

	| aPaths somePathSpecs |
	theAllSpecs isNil  ifTrue: [ ^nil].
	
	somePathSpecs := OrderedCollection new: theAllSpecs size.

	theAllSpecs do: [ :aSpec |  
		aSpec isTerminalChildSpec 
			ifFalse: [ somePathSpecs add: aSpec]
	].
		
	aPaths := self preferredCacheEntryClass new metaSelectors: somePathSpecs.

	^aPaths!

buildPerspectivesFromSpecs: theAllSpecs

	| somePerspectives aPerspective someTerminalEditorSpecs someSeparatePerspectiveSpecs someSpecs aPerspectiveCounter somePerspectiveSpecs aMaxNumberOfSpecsInPerspective aMaxNumberOfSpecsInCreationDialogPerspective aMaxNumberOfSpecs |

	(theAllSpecs isNil  or: [ theAllSpecs isEmpty]) ifTrue: [ ^nil].

	someTerminalEditorSpecs := OrderedCollection new: theAllSpecs size.
	someSeparatePerspectiveSpecs := OrderedCollection new: theAllSpecs size.

	self classifySpecs: theAllSpecs intoTerminals: someTerminalEditorSpecs separate: someSeparatePerspectiveSpecs.

	(someTerminalEditorSpecs size = 1 and: [ someSeparatePerspectiveSpecs size = 1 and: [ 
		someTerminalEditorSpecs first ==  someSeparatePerspectiveSpecs first]]) ifTrue: [ 
		someSeparatePerspectiveSpecs := OrderedCollection new
	].

	aMaxNumberOfSpecsInPerspective := self maxNumberOfSpecsInEachPerspectiveParmValue.
	aMaxNumberOfSpecsInCreationDialogPerspective := self maxNumberOfSpecsInEachCreationDialogPerspectiveParmValue.

	aMaxNumberOfSpecs := (theAllSpecs first isCMDriven and: [ theAllSpecs first  metaInfo type name endsWith:  CODEElement requiredFeaturesTypePostfix]) 
		ifTrue: [ aMaxNumberOfSpecsInCreationDialogPerspective]
		ifFalse: [ aMaxNumberOfSpecsInPerspective].

	somePerspectives := OrderedCollection new.
	someSpecs := someTerminalEditorSpecs copy.
	aPerspectiveCounter := 1.
	[someSpecs isEmpty] whileFalse: [ 
		someSpecs size > aMaxNumberOfSpecs 
			ifTrue: [ 
				somePerspectiveSpecs := someSpecs copyFrom: 1 to: aMaxNumberOfSpecs.
				someSpecs :=  someSpecs copyFrom: aMaxNumberOfSpecs + 1 to: someSpecs size.
			]
			ifFalse: [ 
				somePerspectiveSpecs := someSpecs.
				someSpecs := OrderedCollection new.
			].
		aPerspective := self preferredPerspectiveSpecClass
			name: self  generalPerspectiveName , (aPerspectiveCounter > 1 ifFalse: [''] ifTrue: [ '_', aPerspectiveCounter printString])
			view: (self preferredCacheEntryClass new metaSelectors: somePerspectiveSpecs).

		somePerspectives add: aPerspective.
		aPerspectiveCounter := aPerspectiveCounter + 1
	].

	someSeparatePerspectiveSpecs do: [:aSpec |
		aPerspective := self preferredCMPerspectiveSpecClass
			name: aSpec name 
			view: (self preferredCacheEntryClass new metaSelectors: (Array with: aSpec)).
		
		Object messageNotUnderstoodSignal handle: [ :anEx | ] do: [ 
			aPerspective 
				nlsGroup: aSpec metaInfo nameNLSGroupName;
				nlsItem: aSpec metaInfo  nameNLSItemName
		].
		Object messageNotUnderstoodSignal handle: [ :anEx | ] do: [ 
			aPerspective metaInfo: aSpec metaInfo
		].

		somePerspectives add: aPerspective
	].

	^somePerspectives!

buildSpecFromAttribute: theAttribute

	| aValueType aNonVirtualType |

	theAttribute isNil ifTrue: [ ^nil].
	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].
	
	aNonVirtualType := aValueType nonVirtualType.

	^(aValueType isPrimitive or: [ aValueType isEnumeration or: [ 
		aNonVirtualType isPrimitive or: [ aNonVirtualType isEnumeration]]]) 
		ifFalse: [ 
			theAttribute isMultiplicityMany 
				ifFalse: [ self buildClassSpecFromAttribute: theAttribute]
				ifTrue: [
					theAttribute isOrdered 
						ifTrue: [ self buildOrderedCollectionSpecFromAttribute: theAttribute]
						ifFalse: [ self buildCollectionSpecFromAttribute: theAttribute]
					]
		]
		ifTrue: [
			theAttribute isMultiplicityMany 
				ifFalse: [ 
					(aNonVirtualType isPrimitiveText or: [ 
						theAttribute hasOrTypeHasAspectNamed: theAttribute class presentationTextAspectName])
						ifTrue: [ self buildTextSpecFromAttribute: theAttribute]
						ifFalse: [ self buildTerminalSpecFromAttribute: theAttribute]
				]
				ifTrue: [ 	
					theAttribute isOrdered 
						ifTrue: [ self buildTerminalOrderedCollectionSpecFromAttribute: theAttribute]
						ifFalse: [ self buildTerminalCollectionSpecFromAttribute: theAttribute]
				]
		]!

buildSpecFromOperation: theOperation

	| anIsStatic aSpec |

	theOperation isNil ifTrue: [ ^nil].

	anIsStatic := false.

	aSpec := self preferredCMOperationVoidNoArgsChildSpecClass  new
		name: theOperation name;
		basicSelector: theOperation name asSymbol;
		type: #Operation;
		displayValue: true;
		isChildren: false;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theOperation name  , ' of a ', theOperation type name;
		displaySelector: nil;
		canShowInTree: true;
		metaInfo:   theOperation;
		nlsGroup: theOperation nameNLSGroupName;
		nlsItem: theOperation nameNLSItemName;
		yourself.

	^aSpec!

buildSpecFromRelationship: theRelationship

	theRelationship isNil ifTrue: [ ^nil].


	theRelationship isMultiplicityMany ifFalse: [
		^self buildClassSpecFromRelationship: theRelationship
	].

	^theRelationship isOrdered 
		ifTrue: [ self buildOrderedCollectionSpecFromRelationship: theRelationship]
		ifFalse: [ self buildCollectionSpecFromRelationship: theRelationship]!

buildTerminalCollectionSpecFromAttribute: theAttribute

	| aSpecClass aValueType aSpecType aSpec anEnumString aStream someEnumAttributes aNonVirtualType anIsStatic |

	theAttribute isNil ifTrue: [ ^nil].

	theAttribute isMultiplicityMany  ifFalse: [ ^nil].
	theAttribute isOrdered  ifTrue: [ ^nil].

	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].
	aNonVirtualType := aValueType nonVirtualType.

	aSpecType := aNonVirtualType isPrimitive
		ifTrue: [ aNonVirtualType name asSymbol]
		ifFalse: [
			aNonVirtualType isEnumeration
				ifTrue: [#Enum]
				ifFalse: [ nil]
		].
	aSpecType  isNil ifTrue: [ ^nil].
	aSpecType = #Integer ifTrue: [ aSpecType := #Number].


	anEnumString := nil.
	aNonVirtualType isEnumeration ifTrue: [ 
		aStream := WriteStream on: (String new: 128).
		someEnumAttributes := aValueType allAttributes.
		someEnumAttributes do: [:anEnumAttrib | aStream nextPutAll: anEnumAttrib name; space].
		anEnumString := aStream contents
	].

	anIsStatic :=  (self canChangeFeature: theAttribute) not or: [ 
		theAttribute computationKind = theAttribute class computationKindAlways ].

	aSpecClass := self preferredTerminalCollectionChildSpecClass.


	aSpec := aSpecClass new
		name: theAttribute name ;
		basicSelector: theAttribute name asSymbol;
		type: aSpecType;
		displayValue: true;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theAttribute name  , ' of a ', theAttribute type name, ' (a ', aValueType name, ')';
		displaySelector: #yourself;
		componentsClassName: #String;
		sortSelector: #yourself;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		autoFilter: true;
		componentsClassName: aSpecType;
		sortSelector: #yourself;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		deletionPolicy: #Default;
		changeTerminalPolicy: #Default;
		changeTerminalMode: #Default;
		enumValuesString: anEnumString;
		metaInfo:   theAttribute;
		nlsGroup: theAttribute nameNLSGroupName;
		nlsItem: theAttribute nameNLSItemName;
		yourself.

	^aSpec!

buildTerminalOrderedCollectionSpecFromAttribute: theAttribute

	| aSpecClass aValueType aSpecType aSpec anEnumString aStream someEnumAttributes aNonVirtualType anIsStatic |

	theAttribute isNil ifTrue: [ ^nil].

	theAttribute isMultiplicityMany  ifFalse: [ ^nil].
	theAttribute isOrdered  ifFalse: [ ^nil].

	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].
	aNonVirtualType := aValueType nonVirtualType.

	aSpecType := aNonVirtualType isPrimitive
		ifTrue: [ aNonVirtualType name asSymbol]
		ifFalse: [
			aNonVirtualType isEnumeration
				ifTrue: [#Enum]
				ifFalse: [ nil]
		].
	aSpecType  isNil ifTrue: [^nil].
	aSpecType = #Integer ifTrue: [ aSpecType := #Number].



	anEnumString := nil.
	aNonVirtualType isEnumeration ifTrue: [ 
		aStream := WriteStream on: (String new: 128).
		someEnumAttributes := aValueType allAttributes.
		someEnumAttributes do: [:anEnumAttrib | aStream nextPutAll: anEnumAttrib name; space].
		anEnumString := aStream contents
	].

	anIsStatic :=  (self canChangeFeature: theAttribute) not or: [ 
		theAttribute computationKind = theAttribute class computationKindAlways ].


	aSpecClass := self preferredCMTerminalOrderedCollectionChildSpecClass.

	aSpec := aSpecClass new
		name: theAttribute name ;
		basicSelector: theAttribute name asSymbol;
		type: aSpecType;
		displayValue: true;
		isChildren: true;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theAttribute name  , ' of a ', theAttribute type name, ' (a ', aValueType name, ')';
		displaySelector: #yourself;
		componentsClassName: #String;
		sortSelector: 	#yourself;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		autoFilter: true;
		componentsClassName: aSpecType;
		deletionPolicy: #Default;
		deletionMode: #Default;
		showChildren: true;
		inheritanceLinkSelector: nil;
		menuSelector: nil;
		deletionPolicy: #Default;
		changeTerminalPolicy: #Default;
		changeTerminalMode: #Default;
		enumValuesString: anEnumString;
		metaInfo:   theAttribute;
		nlsGroup: theAttribute nameNLSGroupName;
		nlsItem: theAttribute nameNLSItemName;

		yourself.

	^aSpec!

buildTerminalSpecFromAttribute: theAttribute

	| aSpecClass aValueType aSpecType aSpec anEnumString aStream someEnumAttributes aNonVirtualValueType anIsStatic |

	theAttribute isNil ifTrue: [ ^nil].

	theAttribute isMultiplicityMany  ifTrue: [ ^nil].

	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].

	aNonVirtualValueType := aValueType nonVirtualType.
	aNonVirtualValueType isNil ifTrue: [ ^nil].
	
	aSpecType := aNonVirtualValueType isPrimitive
		ifTrue: [ aNonVirtualValueType primitiveBroker specType]
		ifFalse: [
			aNonVirtualValueType isEnumeration
				ifTrue: [#Enum]
				ifFalse: [ nil]
		].

	aSpecType  isNil ifTrue: [^nil].
	aSpecType = #Integer ifTrue: [ aSpecType := #Number].


	anEnumString := nil.
	aValueType isEnumeration ifTrue: [ 
		aStream := WriteStream on: (String new: 128).
		someEnumAttributes := aValueType allAttributes.
		someEnumAttributes do: [:anEnumAttrib | aStream nextPutAll: anEnumAttrib name; space].
		anEnumString := aStream contents
	].

	aSpecClass := self preferredCMTerminalChildSpecClass.

	anIsStatic := (self canChangeFeature: theAttribute)  not or: [ 
		theAttribute computationKind = theAttribute class computationKindAlways].

	aSpec := aSpecClass new
		name: theAttribute name;
		basicSelector: theAttribute name asSymbol;
		type: aSpecType;
		displayValue: true;
		isChildren: false;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theAttribute name  , ' of a ', theAttribute type name, ' (a ', aValueType name, ')';
		displaySelector: nil;
		canShowInTree: true;
		enumValuesString: anEnumString;
		metaInfo:   theAttribute;
		nlsGroup: theAttribute nameNLSGroupName;
		nlsItem: theAttribute nameNLSItemName;
		yourself.

	^aSpec!

buildTextSpecFromAttribute: theAttribute

	| aSpecClass aValueType aSpec aNonVirtualValueType anIsStatic |

	theAttribute isNil ifTrue: [ ^nil].

	theAttribute isMultiplicityMany  ifTrue: [ ^nil].

	aValueType := theAttribute valueType.
	aValueType isNil ifTrue: [ ^nil].

	aNonVirtualValueType := aValueType nonVirtualType.
	aNonVirtualValueType isNil ifTrue: [ ^nil].
	
	(aValueType isPrimitiveText not and: [ aNonVirtualValueType isPrimitiveText not and: [
		aValueType isPrimitiveString not and: [ aNonVirtualValueType isPrimitiveString not]
	]]) ifTrue: [ ^nil].

	aSpecClass := self preferredCMTextChildSpecClass.

	anIsStatic := theAttribute isChangeable not or: [ 
		theAttribute computationKind = theAttribute class computationKindAlways].

	aSpec := aSpecClass new
		name: theAttribute name;
		basicSelector: theAttribute name asSymbol;
		type: #Text;
		displayValue: true;
		isChildren: false;
		isStatic: anIsStatic;
		creationPolicy: #Default;
		creationMode: #Create;
		helpString: theAttribute name  , ' of a ', theAttribute type name, ' (a ', aValueType name, ')';
		displaySelector: nil;
		canShowInTree: true;
		enumValuesString: nil;
		metaInfo:   theAttribute;
		nlsGroup: theAttribute nameNLSGroupName;
		nlsItem: theAttribute nameNLSItemName;
		yourself.

	^aSpec!

buildViewFromSpecs: theAllSpecs

	| aView |
	theAllSpecs isNil ifTrue: [ ^nil].

	aView := self preferredCacheEntryClass new metaSelectors:  theAllSpecs.
	^aView!

buildViews: theViews perspectives: thePerspectives paths: thePaths fromModel: theModel

	^self buildViews: theViews perspectives: thePerspectives paths: thePaths fromModule: theModel!

buildViews: theViews perspectives: thePerspectives paths: thePaths fromModule: theModule

	| someTypes someSubModules |
	theModule isNil ifTrue: [ ^self].

	someTypes := theModule types.
	someTypes isNil ifFalse: [ 
		someTypes do: [:aType |
			self   buildViews: theViews perspectives: thePerspectives paths: thePaths fromType: aType
		]
	].

	someSubModules := theModule subModules.
	someSubModules isNil ifFalse: [ 
		someSubModules do: [:aSubModule |
			self   buildViews: theViews perspectives: thePerspectives paths: thePaths fromModule: aSubModule
		]
	].!

buildViews: theViews perspectives: thePerspectives paths: thePaths fromType: theType


	| someAttributes  someRelationships  aView somePerspectives someLocalAttributeSpecs someLocalRelationshipSpecs aPaths someAllSpecs aMetaInfoSpec someEffectiveFeatures aDomainSpec someLocalAttributeSpecsWODomain someLocalOperationSpecs someOperations |

	theType isNil ifTrue: [ ^self].

	((theViews at: theType ifAbsent: [ nil]) notNil or: [ 
		(thePerspectives at: theType ifAbsent: [ nil]) notNil or: [ 
			(thePaths at: theType ifAbsent: [ nil]) notNil]])  ifTrue: [ ^self].


	someEffectiveFeatures := theType allEffectiveStructuralFeatures.

	someLocalAttributeSpecs := OrderedCollection new.
	someAttributes := someEffectiveFeatures select: [:aFeature | aFeature isAttribute].
	(someAttributes isNil or: [ someAttributes isEmpty]) ifFalse: [ 
		someAttributes do: [:anAttribute |  | anAttributeSpec |
			anAttributeSpec := self buildSpecFromAttribute: anAttribute.
			anAttributeSpec isNil  ifFalse: [ someLocalAttributeSpecs add: anAttributeSpec].
		]
	].

	someLocalRelationshipSpecs := OrderedCollection new.
	someRelationships := someEffectiveFeatures select: [:aFeature | aFeature isRelationship].
	(someRelationships isNil or: [ someRelationships isEmpty]) ifFalse: [ 
		someRelationships do: [:aRelationship | | aRelationshipSpec |
			aRelationshipSpec := self buildSpecFromRelationship: aRelationship.
			aRelationshipSpec isNil  ifFalse: [ someLocalRelationshipSpecs add: aRelationshipSpec]
		]
	].

	someLocalOperationSpecs := OrderedCollection new.
	someOperations :=  theType allEffectiveOperations.
	(someOperations isNil or: [ someOperations isEmpty]) ifFalse: [ 
		someOperations do: [:anOperation |  | anOperationSpec |
			anOperationSpec := self buildSpecFromOperation: anOperation.
			anOperationSpec isNil  ifFalse: [ someLocalOperationSpecs add: anOperationSpec].
		]
	].


	aDomainSpec := someLocalAttributeSpecs detect: [:aSpec | aSpec name = CODEElement objectDomainCMGOAttributeName] ifNone: [ nil].
	someLocalAttributeSpecsWODomain := aDomainSpec isNil 
		ifTrue: [ someLocalAttributeSpecs] 
		ifFalse: [ someLocalAttributeSpecs copyWithout: aDomainSpec].

	aMetaInfoSpec := self buildMetaInfoSpec.
	
	someAllSpecs :=  someLocalAttributeSpecsWODomain, someLocalOperationSpecs, someLocalRelationshipSpecs, 
		(aMetaInfoSpec isNil ifTrue:  [ Array new] ifFalse: [ Array with: aMetaInfoSpec]),
		(aDomainSpec isNil ifTrue: [ Array new] ifFalse: [ Array with: aDomainSpec]).

	aView := self buildViewFromSpecs: someAllSpecs.
	aView isNil ifFalse: [ theViews at: theType put: aView].


	somePerspectives := self buildPerspectivesFromSpecs: someAllSpecs.
	somePerspectives isNil ifFalse: [  thePerspectives at: theType put: somePerspectives ].

	aPaths := self buildPathsFromSpecs: someAllSpecs.
	aPaths isNil ifFalse: [ thePaths at: theType put: aPaths].!

canChangeFeature: theFeature

	theFeature isNil ifTrue: [ ^false].

	^theFeature isChangeable!

classifySpecs: theAllSpecs intoTerminals: theTerminalEditorSpecs separate: theSeparatePerspectiveSpecs

	theAllSpecs do: [ :aSpec |  
		aSpec isTerminalChildSpec ifTrue: [ 
			aSpec type = #Text ifTrue: [ theSeparatePerspectiveSpecs add: aSpec].
			 theTerminalEditorSpecs add: aSpec
		].
		aSpec isClassChildSpec ifTrue: [ 
			aSpec showInEditor ifTrue: [ theTerminalEditorSpecs add: aSpec].
			theSeparatePerspectiveSpecs add: aSpec
		].
		aSpec isCollectionChildSpec ifTrue: [ 
			theSeparatePerspectiveSpecs add: aSpec
		].
		aSpec isOperationChildSpec ifTrue: [ 
			 theTerminalEditorSpecs add: aSpec
		].
	].! !

!CMDefinitionsHolder publicMethodsFor: 'accessing'!

model
	^model!

model: theModule
	model := theModule.
	self changed: #model! !

!CMDefinitionsHolder publicMethodsFor: 'app models'!

applicationModelAndSpecFor: theObject
	
	| aMetaInfo |
	(theObject isKindOf: CMGenericObject) ifFalse: [ 
		^super applicationModelAndSpecFor: theObject
	].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^super metaSelectorsFor: theObject].

	^self applicationModels at: aMetaInfo ifAbsent: [nil]! !

!CMDefinitionsHolder publicMethodsFor: 'filters'!

filterFor: theObject metaSelector: theChildSpec

	| someFilters aFilter aFilterSpec aMetaInfo |

	(theObject isKindOf: CMGenericObject) ifFalse: [ 
		^super filterFor: theObject metaSelector: theChildSpec
	].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^super filterFor: theObject metaSelector: theChildSpec].

	aFilter := nil.

	someFilters := self filters at: aMetaInfo ifAbsent: [].
	someFilters isNil ifTrue: [
		someFilters := Dictionary new.
		self filters at: aMetaInfo put: someFilters].

	aFilter := someFilters at: aMetaInfo name ifAbsent: [nil].

	aFilter isNil ifTrue: [
		aFilterSpec  := theChildSpec generateAutoFilterInDefinitionsHolder: self.
		aFilterSpec isNil ifFalse: [
			aFilter := aFilterSpec  asFilter.
			aFilter isNil ifFalse: [ someFilters at: theChildSpec put: aFilter]]].

	^aFilter! !

!CMDefinitionsHolder publicMethodsFor: 'incremental'!

ensureHasViewForType: theType
	| someViews somePerspectives somePaths |
	theType isNil ifTrue: [ ^self].

	((self views at: theType ifAbsent: [ nil]) notNil or: [ 
		(self perspectives at: theType ifAbsent: [ nil]) notNil or: [ 
			(self paths at: theType ifAbsent: [ nil]) notNil]])  ifTrue: [ ^self].

	someViews 			:= self views.
	somePerspectives	:= self perspectives.
	somePaths 			:= self paths.

	self class buildViews: someViews perspectives: somePerspectives paths: somePaths fromType: theType.! !

!CMDefinitionsHolder publicMethodsFor: 'list menus'!

listMenuAndControlFor: theObject metaSelector: theChildSpec

	| someListMenuAndControls aMetaInfo aFeatureMetaInfo |
	(theObject isKindOf: CMGenericObject) ifFalse: [ 
		^super listMenuAndControlFor: theObject metaSelector: theChildSpec
	].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^super listMenuAndControlFor: theObject metaSelector: theChildSpec].

	someListMenuAndControls := self listMenus at: aMetaInfo ifAbsent: [nil].
	someListMenuAndControls isNil ifTrue: [^nil].

	aFeatureMetaInfo := theChildSpec metaInfo.
	
	^someListMenuAndControls at: aFeatureMetaInfo name ifAbsent: [nil].! !

!CMDefinitionsHolder publicMethodsFor: 'meta selectors'!

metaSelectorsFor: theObject

	| aMetaInfo someSelectors |
	(theObject isKindOf: CMGenericObject) ifFalse: [ 
		^super metaSelectorsFor: theObject
	].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^super metaSelectorsFor: theObject].

	someSelectors := self metaSelectorsForMetaInfo:  aMetaInfo.
	^someSelectors!

metaSelectorsForMetaInfo: theMetaInfo

	| aView someSelectors |
	theMetaInfo isNil ifTrue: [ ^nil].

	aView := self views at: theMetaInfo  ifAbsent: [nil].
	aView isNil ifTrue: [ ^#() copy].

	someSelectors := aView metaSelectors.
	^someSelectors! !

!CMDefinitionsHolder publicMethodsFor: 'paths'!

pathSelectorsFor: theObject

	| aMetaInfo someSelectors aView |

	(theObject isKindOf: CMGenericObject) ifFalse: [ 
		^super pathSelectorsFor: theObject
	].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^super metaSelectorsFor: theObject].

	aView := self paths at: aMetaInfo  ifAbsent: [nil].
	aView isNil ifTrue: [ ^#() copy].

	someSelectors := aView metaSelectors.
	someSelectors isNil ifTrue: [ ^#() copy].

	^someSelectors select: [:aChildSpec | aChildSpec isCollectionChildSpec or: [
		aChildSpec isClassChildSpec or: [aChildSpec isTreeChildSpec]]]! !

!CMDefinitionsHolder publicMethodsFor: 'persp. app models'!

perspectiveApplicationModelAndSpecFor: theObject perspective: thePerspectiveName

	| someApplicationModelsAndSpecs  aMetaInfo |
	(theObject isKindOf: CMGenericObject) ifFalse: [ 
		^super perspectiveApplicationModelAndSpecFor: theObject perspective: thePerspectiveName
	].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^super perspectiveApplicationModelAndSpecFor: theObject perspective: thePerspectiveName].

	someApplicationModelsAndSpecs :=  self perspectivesApplicationModels at: aMetaInfo ifAbsent: [nil].
	someApplicationModelsAndSpecs isNil ifTrue: [^nil].
	^someApplicationModelsAndSpecs at: thePerspectiveName ifAbsent: [ nil]! !

!CMDefinitionsHolder publicMethodsFor: 'perspectives'!

perspectivesFor: theObject

	| aMetaInfo |

	(theObject isKindOf: CMGenericObject) ifFalse: [ 
		^super perspectivesFor: theObject
	].

	aMetaInfo := theObject metaInfo.
	aMetaInfo isNil ifTrue: [ ^super perspectivesFor: theObject].

	^self perspectives at: aMetaInfo ifAbsent: [nil]! !

CMDefinitionsHolder initializeAfterLoad!
CODE_META_Support initializeAfterLoad!

CODE_META_Support loaded!
