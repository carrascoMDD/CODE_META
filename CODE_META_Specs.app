'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Specs in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Specs becomeDefault!

METAClassChildSpec subclass: #CMClassChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METACollectionChildSpec subclass: #CMCollectionChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METAOrderedCollectionChildSpec subclass: #CMOrderedCollectionChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METATerminalCollectionChildSpec subclass: #CMTerminalCollectionChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METATerminalOrderedCollectionChildSpec subclass: #CMTerminalOrderedCollectionChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METAOperationVoidNoArgsChildSpec subclass: #CMOperationVoidNoArgsChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METATerminalChildSpec subclass: #CMTerminalChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METADiagramChildSpec subclass: #CMDiagramChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METAImageChildSpec subclass: #CMImageChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METATextChildSpec subclass: #CMTextChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METACodeChildSpec subclass: #CMCodeChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METAHyperTextChildSpec subclass: #CMHyperTextChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METATreeChildSpec subclass: #CMTreeChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METATreeEditChildSpec subclass: #CMTreeEditChildSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

METAPerspectiveSpec subclass: #CMPerspectiveSpec
	instanceVariableNames: 'metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

SubApplication subclass: #CODE_META_Specs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Specs becomeDefault!

!CMClassChildSpec class publicMethodsFor: 'preferences'!

preferredCMAspectAdaptorWithCheckClass
	^self preferredPreferencesClass preferredCMAspectAdaptorWithCheckClass!

preferredCMClassAspectAdaptorWithCheckClass
	^self preferredPreferencesClass preferredCMClassAspectAdaptorWithCheckClass!

preferredPreferencesClass
	^CMPreferences! !

!CMClassChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMClassChildSpec publicMethodsFor: 'adaptors'!

buildClassAdaptors

	| anAdaptor otherAdaptor |
	anAdaptor := self preferredCMAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.
	anAdaptor isReadOnly: true.
		
	otherAdaptor := self preferredCMClassAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.

	otherAdaptor subjectChannel: anAdaptor.
	otherAdaptor subjectSendsUpdates: true.
	otherAdaptor isReadOnly: true.

	^Array with: anAdaptor with: otherAdaptor!

linkActionBlockOnAdaptor: theAdaptor

	theAdaptor isNil ifTrue: [ ^nil].

	^ [:aObjectHolder |

		(theAdaptor subjectChannel isNil not and: [
			theAdaptor subjectChannel value isNil not]) ifTrue: [
			self addNewObjectForParentObject:  theAdaptor subjectChannel value objectHolder: aObjectHolder.
		]
	]!

unlinkActionBlockOnAdaptor: theAdaptor

	theAdaptor isNil ifTrue: [ ^nil].

	^ [
		   | aFeatureMetaInfo anExistingObject |
		
		(theAdaptor subjectChannel isNil not and: [
			theAdaptor subjectChannel value isNil not and: [ theAdaptor value isNil not]]) ifTrue: [

			anExistingObject := theAdaptor value.
			anExistingObject isNil ifFalse: [ 
				aFeatureMetaInfo := self metaInfo.
				aFeatureMetaInfo isNil ifFalse: [ 
					aFeatureMetaInfo object: theAdaptor subjectChannel value unsetTC: anExistingObject.
				].
			]
		]
	]! !

!CMClassChildSpec publicMethodsFor: 'configuration'!

discardPerspectivesInCreationDialogsParameterValueForNode: theNode
	theNode isNil ifTrue: [ ^false].! !

!CMClassChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMClassChildSpec publicMethodsFor: 'nodes'!

addNewObjectForParentObject: theParentObject objectHolder: theObjectHolder

	|   aFeatureMetaInfo aNewObject aRequiredFeaturesObject someCandidateFactoryTypes aSelectedFactoryType aSelectedCandidate someCandidates someValuesAndLabels someSortedCandidateValuesAndLabels |

	self isStatic ifTrue: [ ^nil].

	theParentObject isNil ifTrue: [  ^nil].
	theObjectHolder isNil ifTrue: [ ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [ ^nil].

	aSelectedCandidate := nil.

	someCandidates := aFeatureMetaInfo candidateReferencedObjects: theParentObject.

	someCandidateFactoryTypes := aFeatureMetaInfo candidateFactoryTypes.

	(someCandidates isNil not and: [ someCandidates isEmpty not]) 
		ifFalse: [ 
			(someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [  
				Dialog warn: 'No existen candidatos para ', aFeatureMetaInfo nlsName.
			 	^self 
			]
		]
		ifTrue: [ 
			someValuesAndLabels := OrderedCollection new: someCandidates size.
			someCandidates do: [:aCandidateObject |  |  aCandidateObjectMetaInfo aCandidateLabel  |
				aCandidateObjectMetaInfo :=  aCandidateObject metaInfo.
				aCandidateObjectMetaInfo isNil ifFalse: [ 
					aCandidateLabel := aCandidateObjectMetaInfo getObjectNameValue: aCandidateObject	.
					aCandidateLabel isNil not ifTrue: [ 
						someValuesAndLabels add: (Array with: aCandidateObject with: aCandidateLabel)
					]
				]
			].
			someValuesAndLabels isEmpty ifFalse: [ 
				someSortedCandidateValuesAndLabels := someValuesAndLabels asSortedCollection: [:a :b | (a at: 2) < (b at: 2)].
 
				aSelectedCandidate := Dialog 
					choose: ('Por favor, seleccione \', aFeatureMetaInfo nlsName) withCRs
					fromList: (someSortedCandidateValuesAndLabels collect: [:aValueAndLabel |  aValueAndLabel at: 2]) 
					values: (someSortedCandidateValuesAndLabels collect: [:aValueAndLabel |  aValueAndLabel first]) 
					buttons: ((someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [ #()] ifFalse: [ #('Crear')])
					values: ((someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [ #()] ifFalse: [ #(#create)])
					lines: (((someSortedCandidateValuesAndLabels size + 1) max: 5) min: 18)
					cancel: [#cancel].
				aSelectedCandidate == #cancel ifTrue: [ ^nil].
				aSelectedCandidate == #create ifTrue: [ aSelectedCandidate := nil].
			]
	].

	aSelectedCandidate isNil 
		ifFalse: [ aNewObject := aSelectedCandidate]
		ifTrue: [ 
			(someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [ ^nil].

			someCandidateFactoryTypes size > 1 
				ifFalse: [ aSelectedFactoryType := someCandidateFactoryTypes asArray first]
				ifTrue: [ 
					someCandidateFactoryTypes := someCandidateFactoryTypes asSortedCollection: [:a :b | a nlsName < b nlsName].
 
					aSelectedFactoryType := Dialog 
						choose: ('Por favor, seleccione el tipo de\', aFeatureMetaInfo nlsName, '\a crear') withCRs
						fromList: (someCandidateFactoryTypes collect: [:aType |  aType nlsName, ' 	(', aType name, ')' ]) 
						values: someCandidateFactoryTypes 
						lines: (((someCandidateFactoryTypes size + 1) max: 5) min: 18)
						cancel: [nil]
				].
	
			aSelectedFactoryType isNil ifTrue: [ ^nil].

			aRequiredFeaturesObject := self editRequiredFeaturesForNewObjectOfType: aSelectedFactoryType inNode: theObjectHolder.
			aRequiredFeaturesObject == false ifTrue: [ ^nil].


			aNewObject := aRequiredFeaturesObject isNil 
				ifTrue: [ aSelectedFactoryType createObject] 
				ifFalse: [ aSelectedFactoryType createObjectWithRequiredFeaturesObject: aRequiredFeaturesObject ].
			aNewObject isNil ifTrue: [  ^nil].
		].


	aFeatureMetaInfo object: theParentObject setTC: aNewObject.

	^aNewObject!

addNewObjectInNode: theNode 

	|   aParentNode aParentObject  |

	self isStatic ifTrue: [ ^nil].

	theNode isNil ifTrue: [ ^nil].

	aParentNode := theNode parent.
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	^self addNewObjectForParentObject: aParentObject objectHolder: theNode!

addNodesForNode: theNode


	| aChildValue aClassHolderNode someChildrenValues |

"	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].
	
	aMetaInfo isAttribute ifTrue: [ 
		aValueType := aMetaInfo valueType.
		aValueType isNil ifFalse: [

			aValueTypeSymbol := aValueType name asSymbol.

			aIsCODEElement := aValueTypeSymbol = CODEType name or: [ 
				aValueTypeSymbol = CODEModel name or: [ 
				aValueTypeSymbol = CODEAttribute name or: [ 
				aValueTypeSymbol = CODERelationship name]]].
	
			aIsCODEElement ifTrue:  [
				self halt: self name.
			
			]
		]
	].
"
	aChildValue	:= self childValueForObject: theNode value.
	someChildrenValues := theNode filterChildrenValues: (Array with: aChildValue).
	(someChildrenValues isNil or: [ someChildrenValues isEmpty]) ifTrue:  [ ^self].

	aClassHolderNode := theNode preferredClassNodeClass new
		browser: theNode browser;  
		definitionsHolder: theNode definitionsHolder;
		value: aChildValue;  
		displaySelector: self displaySelector;
		childSpec: (aChildValue isNil ifTrue: [ self ] ifFalse: [theNode definitionsHolder  metaSelectorsFor:  aChildValue]);
		classChildSpec: self.

	aChildValue isNil ifFalse: [  theNode  addDependent: aClassHolderNode toObject:  aChildValue].

	theNode addChild: aClassHolderNode. 

	aClassHolderNode updateNodeName.!

childValueForObject: theObject
			
	^self childValueForObject: theObject ifNotUnderstood: [:anObject |
		DEBUGDvpt ifTrue: [
			Transcript show: 'Not understood : ',  anObject metaInfo name , '>', self metaInfo name ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		nil]!

childValueForObject: theObject ifNotUnderstood: theBlock
			
	| aFeatureMetaInfo anObjectMetaInfo aChildValue |

	theObject isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil object in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'CollectionChildSpec without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	anObjectMetaInfo := theObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	(anObjectMetaInfo hasOrInheritsFeature: aFeatureMetaInfo) ifFalse: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object type metaInfo does not include CollectionChildSpec feature metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	aChildValue := aFeatureMetaInfo getObjectFeatureValueTC: theObject.

	^aChildValue!

editRequiredFeaturesForNewObjectOfType: theFactoryType inNode: theNode
	
	| aFeatureMetaInfo aRequiredFeaturesTypeMetaInfo aRequiredFeaturesObject aDefinitionsHolder  aParentNode aParentObject aBrowserParameters aDiscardPerspectives aShowSeparateTextPerspectives |

	self isStatic ifTrue: [ ^nil].

	theFactoryType isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	aParentNode := (theNode isKindOf: METAObjectHolder) ifTrue: [ theNode owner] ifFalse: [ theNode parent].
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aRequiredFeaturesTypeMetaInfo := theFactoryType requiredFeaturesTypeMetaInfo.
	(aRequiredFeaturesTypeMetaInfo isNil or: [ aRequiredFeaturesTypeMetaInfo attributes isEmpty]) ifTrue: [  ^nil].

	aRequiredFeaturesObject := aRequiredFeaturesTypeMetaInfo createRequiredFeaturesObjectForParent: aParentObject 
		fromFeatureMetaInfo: aFeatureMetaInfo.
	aRequiredFeaturesObject isNil ifTrue: [ ^nil].

	aDefinitionsHolder := theNode definitionsHolder.
	aDefinitionsHolder isNil ifTrue: [ ^nil].

	aBrowserParameters := theNode browserParameters.
	aBrowserParameters isNil ifTrue:  [
		 aBrowserParameters :=  Dictionary new: 13.
	].

	aDiscardPerspectives := aRequiredFeaturesTypeMetaInfo getDiscardPerspectivesInCreationDialogsParameterValue.
	aShowSeparateTextPerspectives := aRequiredFeaturesTypeMetaInfo getAlwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue.

	aBrowserParameters 
		at:  METABrowser showCanvasLabelParameterSymbol put: true;
		at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
		at:  METABrowser numberOfEditorHoldersParameterSymbol put: 1;
		at:  METABrowser initialVerticalCanvasesProportionParameterSymbol put: 0;
		at:  METABrowser discardPerspectivesParameterSymbol put: aDiscardPerspectives;
		at:  METABrowser discardTextPerspectivesParameterSymbol put: aShowSeparateTextPerspectives not.

	aDefinitionsHolder ensureHasViewForType: aRequiredFeaturesTypeMetaInfo.

	[| aBrowserClass aDialogResult someIssues |
		aBrowserClass := aDefinitionsHolder preferredApplicationBrowserClass.
		aDialogResult := aBrowserClass
			openForObject: 			aRequiredFeaturesObject 
			definitionsHolder: 		aDefinitionsHolder
			browserParameters:	aBrowserParameters
			beDialog: true
			selectionOn: nil asValue.

		aDialogResult == true ifFalse: [ ^false].
	
		someIssues := aRequiredFeaturesTypeMetaInfo validateRequiredFeaturesObject: aRequiredFeaturesObject.
		(someIssues isNil or: [ someIssues isEmpty ])
			ifTrue: [ ^aRequiredFeaturesObject]
			ifFalse: [ 
				SimpleDialog new
					display: ('Por favor, revise la informacion introducida en los campos : ') withCRs
					fromList: (someIssues collect: [:anIssue |  anIssue first nlsName, ' : ', (anIssue at: 2) ]) 
					values: someIssues 
					lines: (((someIssues size + 1) max: 5) min: 18)
					cancel: [ ^false]
			]
	] repeat.!

getNameForNode: theNode

	| aName anObject aParent aParentValue aNLSName aShowClassNodePrefixes aIsPathFinder aPrefix aBrowserParameters |

	aNLSName := self nlsName.

	self displayValue ifFalse: [ ^aNLSName].
	
	aShowClassNodePrefixes := true.
	aIsPathFinder := theNode browser isNil not and: [ theNode browser topEditor isPathFinder].
	aIsPathFinder ifTrue: [
		aBrowserParameters := theNode browserParameters.
		aBrowserParameters isNil ifFalse:  [ 
			aShowClassNodePrefixes := (aBrowserParameters at: METABrowser showClassNodePrefixesInPathFinderSymbol ifAbsent: [ false]) == true
		]
	].

	aPrefix := aShowClassNodePrefixes ifTrue: [ aNLSName , ':' ] ifFalse: [ '' copy].

	aParent := theNode parent.
	aParent isNil
		ifTrue: [  anObject := theNode value]
		ifFalse: [   
			aParentValue := theNode parent value.
			anObject := self metaInfo getObjectFeatureValueTC: aParentValue.
		].

	anObject isNil ifTrue: [ ^aPrefix, ' (none)'].

	aName := self getNameForObject:  anObject.

	^aName isNil 
		ifTrue: [ aPrefix, ' (no name)']
		ifFalse: [
			aPrefix,  (self firstLineOf: aName contractedTo: 80)
		]!

getNameForObject: theObject

	| aName  |

	self displayValue ifFalse: [ ^self name].

	theObject isNil ifTrue: [ ^nil].

	aName := theObject metaInfo getObjectNameValue: theObject.

	^aName!

getNameForParentObject: theObject

	^self getNameForObject: theObject!

removeNode: theNode

	| aParentNode aParentObject      aNodeValue aFeatureMetaInfo |

	self isStatic ifTrue: [ ^self].

	theNode isNil ifTrue: [ ^self].

	aNodeValue := theNode value.
	aNodeValue isNil ifTrue: [ ^self].

	aParentNode := theNode parent.
	aParentNode isNil ifTrue: [ ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [ ^self].

	aFeatureMetaInfo object: aParentObject unsetTC: aNodeValue.! !

!CMClassChildSpec publicMethodsFor: 'preferences'!

preferredCMAspectAdaptorWithCheckClass 
	^self class preferredCMAspectAdaptorWithCheckClass!

preferredCMClassAspectAdaptorWithCheckClass 
	^self class preferredCMClassAspectAdaptorWithCheckClass! !

!CMClassChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMClassChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMCodeChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMCodeChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMCodeChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMCodeChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMCollectionChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMCollectionChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMCollectionChildSpec publicMethodsFor: 'filters'!

	generateAutoFilter
	| aComponentsClassName aClass aMETASelectorsSelector someChildSpec aFilterSpec |


	self autoFilter == true ifFalse: [^nil].

	aComponentsClassName := self componentsClassName.
	aComponentsClassName isNil ifTrue: [ ^nil].
	
	aClass := Smalltalk at: aComponentsClassName asSymbol ifAbsent: [nil].
	aClass isNil ifTrue: [  
		Dialog warn: 'Class ', aComponentsClassName , ' Not in System'.
		^nil].


	aMETASelectorsSelector  := self  preferredVirtualChildSpecClass metaSelectorsSelector.
	aMETASelectorsSelector isNil ifTrue: [^nil].

	someChildSpec := self tryToSend: aMETASelectorsSelector to: aClass.
	someChildSpec isNil ifTrue: [^nil].
	
	someChildSpec := someChildSpec select: [:aChildSpec | aChildSpec isTerminalChildSpec].

	someChildSpec isEmpty ifTrue: [^nil].

	aFilterSpec := self preferredSimpleFilterSpecClass new.

	someChildSpec do: [:aChildSpec | | aTestSpec |
self halt.
		aTestSpec := self preferredSimpleTestSpecClass new.
		aTestSpec childSpec: aChildSpec.

		aFilterSpec addTest: aTestSpec].

	^aFilterSpec!

generateAutoFilterInDefinitionsHolder: theDefinitionsHolder
	| aComponentsClassName someChildSpec aFilterSpec aMetaInfo aRelatedType |

	theDefinitionsHolder isNil ifTrue: [ ^nil].

	self autoFilter == true ifFalse: [^nil].

	aComponentsClassName := self componentsClassName.
	aComponentsClassName isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aRelatedType := aMetaInfo referencedType.

	someChildSpec := theDefinitionsHolder metaSelectorsForMetaInfo: aRelatedType.
	someChildSpec isNil ifTrue: [^nil].
	
	someChildSpec := someChildSpec select: [:aChildSpec | aChildSpec isTerminalChildSpec].

	someChildSpec isEmpty ifTrue: [^nil].

	aFilterSpec := self preferredSimpleFilterSpecClass new.

	someChildSpec do: [:aChildSpec | | aTestSpec |
		aTestSpec := self preferredSimpleTestSpecClass new.
		aTestSpec childSpec: aChildSpec.

		aFilterSpec addTest: aTestSpec].

	^aFilterSpec! !

!CMCollectionChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMCollectionChildSpec publicMethodsFor: 'nodes'!

addNewObjectInNode: theNode 
	
	| aParentNode aParentObject aFeatureMetaInfo aNewObject aRequiredFeaturesObject someCandidateFactoryTypes aSelectedFactoryType |

	self isStatic ifTrue: [ ^nil].

	aParentNode := theNode parent.
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	someCandidateFactoryTypes := aFeatureMetaInfo candidateFactoryTypes.
	(someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [ ^nil].

	someCandidateFactoryTypes size > 1 
		ifFalse: [ aSelectedFactoryType := someCandidateFactoryTypes asArray first]
		ifTrue: [ 
			someCandidateFactoryTypes := someCandidateFactoryTypes asSortedCollection: [:a :b | a nlsName < b nlsName].
 
			aSelectedFactoryType := Dialog 
				choose: ('Por favor, seleccione el tipo de\', aFeatureMetaInfo nlsName, '\a crear') withCRs
				fromList: (someCandidateFactoryTypes collect: [:aType |  aType nlsName, ' 	(', aType name, ')' ]) 
				values: someCandidateFactoryTypes 
				lines: (((someCandidateFactoryTypes size + 1) max: 5) min: 18)
				cancel: [nil]
		].

	aSelectedFactoryType isNil ifTrue: [ ^nil].

	aRequiredFeaturesObject := self editRequiredFeaturesForNewObjectOfType: aSelectedFactoryType inNode: theNode.
	aRequiredFeaturesObject == false ifTrue: [ ^nil].

	aNewObject := aRequiredFeaturesObject isNil 
		ifTrue: [ aSelectedFactoryType createObject] 
		ifFalse: [ aSelectedFactoryType createObjectWithRequiredFeaturesObject: aRequiredFeaturesObject ].
	aNewObject isNil ifTrue: [  ^nil].
					
	aFeatureMetaInfo object: aParentObject addTC: aNewObject.!

addNodesForNode: theNode 


	| aNode  someChildrenValues someChildrenNodes  aChildNode   aShowInherited someSortedChildrenNodes |

	aNode := (theNode preferredCollectionNodeClass new) 
		browser: theNode browser; 
		childSpec: self;
		definitionsHolder: theNode definitionsHolder.
	aNode initEmptyChildren.
	theNode addChild: aNode.
	aNode updateNodeName.
	aNode initFilter.

	theNode addDependent: aNode toObject: theNode value.

	someChildrenValues	:= self childrenValuesForObject: theNode value.
	someChildrenValues := theNode filterChildrenValues: someChildrenValues.

	someChildrenValues isNil ifTrue: [ someChildrenValues := OrderedCollection new].

	someChildrenNodes	:= OrderedCollection new.

	someChildrenValues do: [:oneValue | 
		aChildNode := theNode preferredNodeClass new 
			browser: theNode browser; 
			value: oneValue;  
			displaySelector: self displaySelector;
			definitionsHolder: theNode definitionsHolder;
			childSpec: (theNode definitionsHolder metaSelectorsFor: oneValue).
		theNode addDependent: aChildNode toObject: oneValue.
		someChildrenNodes addLast: aChildNode].

	self recurseInheritedNode: aNode do: [:anObject :otherChildrenValues :aChildSpec| 
		theNode addDependent: theNode toObject: anObject.

		theNode addInheritanceValue: anObject forLinkSelector: aChildSpec inheritanceLinkSelector.

		otherChildrenValues do: [:oneValue | 
			aChildNode := theNode preferredNodeClass new 
				browser: theNode browser; 
				value: oneValue;   
				displaySelector: self displaySelector;
				definitionsHolder: theNode definitionsHolder;
				childSpec: (theNode definitionsHolder metaSelectorsFor: oneValue);
				isInherited: true.
			theNode addDependent: aChildNode toObject: oneValue.
			someChildrenNodes addLast: aChildNode]].

	aShowInherited :=  theNode browser isNil not and: [ theNode browser showInherited].

	someSortedChildrenNodes := self sortNodes: someChildrenNodes showInherited: aShowInherited.

	someSortedChildrenNodes do: [:oneNode |
		aNode addChild: oneNode.
		oneNode updateNodeName].
	^self!

childrenValuesForObject: theObject
			
	^self childrenValuesForObject: theObject ifNotUnderstood: [:anObject |
		DEBUGDvpt ifTrue: [
			Transcript show: 'Not understood : ',  anObject metaInfo name , '>', self metaInfo name ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		nil]!

childrenValuesForObject: theObject ifNotUnderstood: theBlock
			
	| someChildrenValues aFeatureMetaInfo anObjectMetaInfo |

	theObject isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil object in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'CollectionChildSpec without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	anObjectMetaInfo := theObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	(anObjectMetaInfo hasOrInheritsFeature: aFeatureMetaInfo ) ifFalse: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object type metaInfo does not include CollectionChildSpec feature metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := aFeatureMetaInfo getObjectFeatureValueTC: theObject.

	someChildrenValues  isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil result from ',  theObject class name , '>', self basicSelector ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := someChildrenValues select: [:otherValue | otherValue yourself isNil not].

	^someChildrenValues!

editRequiredFeaturesForNewObjectOfType: theFactoryType inNode: theNode
	
	| aFeatureMetaInfo aRequiredFeaturesTypeMetaInfo aRequiredFeaturesObject aDefinitionsHolder  aParentNode aParentObject aBrowserParameters aDiscardPerspectives aShowSeparateTextPerspectives |

	self isStatic ifTrue: [ ^nil].

	theFactoryType isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	aParentNode := (theNode isKindOf: METAObjectHolder) ifTrue: [ theNode owner] ifFalse: [ theNode parent].
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aRequiredFeaturesTypeMetaInfo := theFactoryType requiredFeaturesTypeMetaInfo.
	(aRequiredFeaturesTypeMetaInfo isNil or: [ aRequiredFeaturesTypeMetaInfo attributes isEmpty]) ifTrue: [  ^nil].

	aRequiredFeaturesObject := aRequiredFeaturesTypeMetaInfo createRequiredFeaturesObjectForParent: aParentObject 
		fromFeatureMetaInfo: aFeatureMetaInfo.
	aRequiredFeaturesObject isNil ifTrue: [ ^nil].

	aDefinitionsHolder := theNode definitionsHolder.
	aDefinitionsHolder isNil ifTrue: [ ^nil].

	aBrowserParameters := theNode browserParameters.
	aBrowserParameters isNil ifTrue:  [
		 aBrowserParameters :=  Dictionary new: 13.
	].

	aDiscardPerspectives := aRequiredFeaturesTypeMetaInfo getDiscardPerspectivesInCreationDialogsParameterValue.
	aShowSeparateTextPerspectives := aRequiredFeaturesTypeMetaInfo getAlwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue.

	aBrowserParameters 
		at:  METABrowser showCanvasLabelParameterSymbol put: true;
		at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
		at:  METABrowser numberOfEditorHoldersParameterSymbol put: 1;
		at:  METABrowser initialVerticalCanvasesProportionParameterSymbol put: 0;
		at:  METABrowser discardPerspectivesParameterSymbol put: aDiscardPerspectives;
		at:  METABrowser discardTextPerspectivesParameterSymbol put: aShowSeparateTextPerspectives not.

	aDefinitionsHolder ensureHasViewForType: aRequiredFeaturesTypeMetaInfo.

	[ | aBrowserClass aDialogResult someIssues |
		aBrowserClass := aDefinitionsHolder preferredApplicationBrowserClass.
		aDialogResult := aBrowserClass
			openForObject: 			aRequiredFeaturesObject 
			definitionsHolder: 		aDefinitionsHolder
			browserParameters:	aBrowserParameters
			beDialog: true
			selectionOn: nil asValue.

		aDialogResult == true ifFalse: [ ^false].
	
		someIssues := aRequiredFeaturesTypeMetaInfo validateRequiredFeaturesObject: aRequiredFeaturesObject.
		(someIssues isNil or: [ someIssues isEmpty ])
			ifTrue: [ ^aRequiredFeaturesObject]
			ifFalse: [ 
				SimpleDialog new
					display: ('Por favor, revise la informacion introducida en los campos : ') withCRs
					fromList: (someIssues collect: [:anIssue |  anIssue first nlsName, ' : ', (anIssue at: 2) ]) 
					values: someIssues 
					lines: (((someIssues size + 1) max: 5) min: 18)
					cancel: [ ^false]
			]
	] repeat.!

insertNode: theNewNode inOrderAmong: theChildrenNodes inParentNode: theParentNode

	| aValue aSortSelector anAfterNode aNewObjectSortValue |
	theNewNode 		isNil ifTrue: [ ^self].
	theChildrenNodes 	isNil ifTrue: [ ^self].
	theParentNode 		isNil ifTrue: [ ^self].

	aValue := theNewNode value.
	aValue 				isNil ifTrue: [ ^self]. 

	self isOrderedCollectionChildSpec ifTrue: [ 
		theParentNode addChild: theNewNode.
		^self
	].

	aSortSelector := self sortSelector.

	aSortSelector isNil ifTrue: [ 
		theParentNode addChild: theNewNode.
		^self
	].

	theChildrenNodes isEmpty ifTrue: [ 
		theParentNode addChild: theNewNode.
		^self
	].
			

	anAfterNode := nil.
	aNewObjectSortValue := (aValue metaInfo featureOrInheritedNamed: aSortSelector asString) getObjectFeatureValueTC: aValue.
	theChildrenNodes detect: [:aChild | 
		((aChild value metaInfo featureOrInheritedNamed: aSortSelector asString) getObjectFeatureValueTC: aValue) <= aNewObjectSortValue
			ifTrue: [ anAfterNode := aChild. false]
			ifFalse: [ true] 
	] ifNone: [nil].

	anAfterNode isNil 
		ifFalse: [ theParentNode addChild: theNewNode after: anAfterNode]
		ifTrue: [ theParentNode addChild: theNewNode before: theChildrenNodes first ].!

mustResortForAspect: theAspect

	^self sortSelector notNil and: [ self sortSelector = theAspect]!

mustSortNodes: theNodes showInherited: theShowInherited


	| aSortSelector  aPrevNode aShowInherited aValue aPrevValue |

	(theNodes isNil or: [ theNodes isEmpty or: [ theNodes size < 2]]) ifTrue: [ ^false].

	aShowInherited := theShowInherited and: [ self inheritanceLinkSelector isNil not].

	aSortSelector := self sortSelector.
	aSortSelector isNil ifTrue: [ ^false].

	aPrevValue := nil.
	aPrevNode := nil.
	^(theNodes detect: [:aNode | | unR |
		aValue := (aNode value metaInfo featureOrInheritedNamed: aSortSelector asString) getObjectFeatureValueTC: aNode value.
		unR := aPrevValue isNil not and: [
		(aPrevValue <= aValue) not or: [ 
			aShowInherited and: [ aNode isInherited and: [ aPrevNode isInherited not]]
		]].
		aPrevNode := aNode. 
		aPrevValue := aValue. 
		unR
	] ifNone: [nil]) isNil not!

removeObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aFeatureMetaInfo |

	self isStatic ifTrue: [ ^nil].

	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self error: 'Can not remove from an unknown collection'].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self error: 'Can not remove an unknown collection element value'].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  
		^self error: 'Can not remove without knowing about the collection node parent node'].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ 
		^self error: 'Can not remove without knowing about the collection node parent node value '].
	 
	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [ 
		^self error: 'Can not remove without knowing about the collection metainfo'].
 
	aFeatureMetaInfo object: aParentObject removeTC: aChildValue.!

sortNodes: theNodes showInherited: theShowInherited


	| aSortSelector  someSortedNodes aShowInherited |

	(theNodes isNil or: [ theNodes isEmpty or: [ theNodes size < 2]]) ifTrue: [ ^theNodes].

	aSortSelector := self sortSelector.
	aSortSelector isNil ifTrue: [ ^theNodes].

	aShowInherited := theShowInherited and: [ self inheritanceLinkSelector isNil not].

	someSortedNodes := theNodes asSortedCollection: [:oneNode :otherNode |    | aValue otherValue |
		aValue := (oneNode value metaInfo featureOrInheritedNamed: aSortSelector asString) getObjectFeatureValueTC: oneNode value.
		otherValue := (otherNode value metaInfo featureOrInheritedNamed: aSortSelector asString) getObjectFeatureValueTC: otherNode value.
		aValue < otherValue
			ifTrue: [true]
			ifFalse: [
				aValue = otherValue
					ifFalse: [false]
					ifTrue: [	
						aShowInherited 
							ifFalse: [false] 
							ifTrue: [oneNode isInherited and: [ otherNode isInherited not]]]]].

	^someSortedNodes asOrderedCollection!

xaddNewObjectInNode: theNode 
	
	| aParentNode aParentObject aFeatureMetaInfo aReferencedTypeMetaInfo aNewObject |

	self isStatic ifTrue: [ ^nil].

	aParentNode := theNode parent.
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aReferencedTypeMetaInfo := aFeatureMetaInfo referencedType.

	aNewObject := aReferencedTypeMetaInfo createObject.
	aNewObject isNil ifTrue: [  ^nil].
					
	aFeatureMetaInfo object: aParentObject addTC: aNewObject.! !

!CMCollectionChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMCollectionChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMDiagramChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMDiagramChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMDiagramChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMDiagramChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMHyperTextChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMHyperTextChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMHyperTextChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMHyperTextChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMImageChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMImageChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMImageChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMImageChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMOperationVoidNoArgsChildSpec class publicMethodsFor: 'preferences'!

preferredCMOperationAdaptorWithCheckClass
	^self preferredPreferencesClass preferredCMOperationAdaptorWithCheckClass!

preferredPreferencesClass
	^CMPreferences! !

!CMOperationVoidNoArgsChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMOperationVoidNoArgsChildSpec publicMethodsFor: 'adaptors'!

buildTargetAdaptor

	^self preferredCMOperationAdaptorWithCheckClass newWithMetaInfo: self metaInfo!

performOperationBlockOnAdaptor: theAdaptor

	theAdaptor isNil ifTrue: [ ^nil].

	^ [:anObjectHolder |
		(theAdaptor subjectChannel isNil not and: [
			theAdaptor subjectChannel value isNil not]) ifTrue: [
			self metaInfo performObjectOperationTC: theAdaptor subjectChannel value 
		]
	]! !

!CMOperationVoidNoArgsChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMOperationVoidNoArgsChildSpec publicMethodsFor: 'nodes'!

getNameForObject: theObject

	| aName  |

	theObject isNil ifTrue: [ ^nil].

	aName := theObject metaInfo getObjectNameValue: theObject.

	^aName!

getNameForParentObject: theObject

	^self getNameForObject: theObject!

nodeNameIncludes: theAspect
	| aMetaInfo |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^false].

	^aMetaInfo name = theAspect! !

!CMOperationVoidNoArgsChildSpec publicMethodsFor: 'preferences'!

preferredCMOperationAdaptorWithCheckClass
	^self class preferredCMOperationAdaptorWithCheckClass! !

!CMOperationVoidNoArgsChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMOrderedCollectionChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMOrderedCollectionChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMOrderedCollectionChildSpec publicMethodsFor: 'filters'!

generateAutoFilter
	| aComponentsClassName aClass aMETASelectorsSelector someChildSpec aFilterSpec |


	self autoFilter == true ifFalse: [^nil].

	aComponentsClassName := self componentsClassName.
	aComponentsClassName isNil ifTrue: [ ^nil].
	
	aClass := Smalltalk at: aComponentsClassName asSymbol ifAbsent: [nil].
	aClass isNil ifTrue: [  
		Dialog warn: 'Class ', aComponentsClassName , ' Not in System'.
		^nil].


	aMETASelectorsSelector  := self  preferredVirtualChildSpecClass metaSelectorsSelector.
	aMETASelectorsSelector isNil ifTrue: [^nil].

	someChildSpec := self tryToSend: aMETASelectorsSelector to: aClass.
	someChildSpec isNil ifTrue: [^nil].
	
	someChildSpec := someChildSpec select: [:aChildSpec | aChildSpec isTerminalChildSpec].

	someChildSpec isEmpty ifTrue: [^nil].

	aFilterSpec := self preferredSimpleFilterSpecClass new.

	someChildSpec do: [:aChildSpec | | aTestSpec |
		aTestSpec := self preferredSimpleTestSpecClass new.
		aTestSpec childSpec: aChildSpec.

		aFilterSpec addTest: aTestSpec].

	^aFilterSpec!

generateAutoFilterInDefinitionsHolder: theDefinitionsHolder
	| aComponentsClassName someChildSpec aFilterSpec aMetaInfo aRelatedType |

	theDefinitionsHolder isNil ifTrue: [ ^nil].

	self autoFilter == true ifFalse: [^nil].

	aComponentsClassName := self componentsClassName.
	aComponentsClassName isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aRelatedType := aMetaInfo referencedType.

	someChildSpec := theDefinitionsHolder metaSelectorsForMetaInfo: aRelatedType.
	someChildSpec isNil ifTrue: [^nil].
	
	someChildSpec := someChildSpec select: [:aChildSpec | aChildSpec isTerminalChildSpec].

	someChildSpec isEmpty ifTrue: [^nil].

	aFilterSpec := self preferredSimpleFilterSpecClass new.

	someChildSpec do: [:aChildSpec | | aTestSpec |
		aTestSpec := self preferredSimpleTestSpecClass new.
		aTestSpec childSpec: aChildSpec.

		aFilterSpec addTest: aTestSpec].

	^aFilterSpec! !

!CMOrderedCollectionChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMOrderedCollectionChildSpec publicMethodsFor: 'nodes'!

addNewObjectForParentObject: theParentObject objectHolder: theObjectHolder

	|   aFeatureMetaInfo aNewObject aRequiredFeaturesObject someCandidateFactoryTypes aSelectedFactoryType aSelectedCandidate someCandidates someValuesAndLabels someSortedCandidateValuesAndLabels |

	self isStatic ifTrue: [ ^nil].

	theParentObject isNil ifTrue: [  ^nil].
	theObjectHolder isNil ifTrue: [ ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [ ^nil].

	aSelectedCandidate := nil.

	someCandidates := aFeatureMetaInfo candidateReferencedObjects: theParentObject.

	someCandidateFactoryTypes := aFeatureMetaInfo candidateFactoryTypes.

	(someCandidates isNil not and: [ someCandidates isEmpty not]) 
		ifFalse: [ 
			(someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [  
				Dialog warn: 'No existen candidatos para ', aFeatureMetaInfo nlsName.
			 	^self 
			]
		]
		ifTrue: [ 
			someValuesAndLabels := OrderedCollection new: someCandidates size.
			someCandidates do: [:aCandidateObject |  |  aCandidateObjectMetaInfo aCandidateLabel  |
				aCandidateObjectMetaInfo :=  aCandidateObject metaInfo.
				aCandidateObjectMetaInfo isNil ifFalse: [ 
					aCandidateLabel := aCandidateObjectMetaInfo getObjectNameValue: aCandidateObject	.
					aCandidateLabel isNil not ifTrue: [ 
						someValuesAndLabels add: (Array with: aCandidateObject with: aCandidateLabel)
					]
				]
			].
			someValuesAndLabels isEmpty ifFalse: [ 
				someSortedCandidateValuesAndLabels := someValuesAndLabels asSortedCollection: [:a :b | (a at: 2) < (b at: 2)].
 
				aSelectedCandidate := Dialog 
					choose: ('Por favor, seleccione \', aFeatureMetaInfo nlsName) withCRs
					fromList: (someSortedCandidateValuesAndLabels collect: [:aValueAndLabel |  aValueAndLabel at: 2]) 
					values: (someSortedCandidateValuesAndLabels collect: [:aValueAndLabel |  aValueAndLabel first]) 
					buttons: ((someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [ #()] ifFalse: [ #('Crear')])
					values: ((someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [ #()] ifFalse: [ #(#create)])
					lines: (((someSortedCandidateValuesAndLabels size + 1) max: 5) min: 18)
					cancel: [#cancel].
				aSelectedCandidate == #cancel ifTrue: [ ^nil].
				aSelectedCandidate == #create ifTrue: [ aSelectedCandidate := nil].
			]
	].

	aSelectedCandidate isNil 
		ifFalse: [ aNewObject := aSelectedCandidate]
		ifTrue: [ 
			(someCandidateFactoryTypes isNil or: [ someCandidateFactoryTypes isEmpty]) ifTrue: [ ^nil].

			someCandidateFactoryTypes size > 1 
				ifFalse: [ aSelectedFactoryType := someCandidateFactoryTypes asArray first]
				ifTrue: [ 
					someCandidateFactoryTypes := someCandidateFactoryTypes asSortedCollection: [:a :b | a nlsName < b nlsName].
 
					aSelectedFactoryType := Dialog 
						choose: ('Por favor, seleccione el tipo de\', aFeatureMetaInfo nlsName, '\a crear') withCRs
						fromList: (someCandidateFactoryTypes collect: [:aType |  aType nlsName, ' 	(', aType name, ')' ]) 
						values: someCandidateFactoryTypes 
						lines: (((someCandidateFactoryTypes size + 1) max: 5) min: 18)
						cancel: [nil]
				].
	
			aSelectedFactoryType isNil ifTrue: [ ^nil].

			aRequiredFeaturesObject := self editRequiredFeaturesForNewObjectOfType: aSelectedFactoryType inNode: theObjectHolder.
			aRequiredFeaturesObject == false ifTrue: [ ^nil].


			aNewObject := aRequiredFeaturesObject isNil 
				ifTrue: [ aSelectedFactoryType createObject] 
				ifFalse: [ aSelectedFactoryType createObjectWithRequiredFeaturesObject: aRequiredFeaturesObject ].
			aNewObject isNil ifTrue: [  ^nil].
		].


	aFeatureMetaInfo object: theParentObject addTC: aNewObject.
	^aNewObject!

addNewObjectInNode: theNode 
	
	| aParentNode aParentObject |
	self isStatic ifTrue: [ ^nil].

	theNode isNil ifTrue: [ ^nil].

	aParentNode := theNode parent.
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	^self addNewObjectForParentObject: aParentObject objectHolder: theNode!

addNodesForNode: theNode 


	| aNode  someChildrenValues someChildrenNodes  aChildNode   |

	aNode := (theNode preferredCollectionNodeClass new) 
		browser: theNode browser; 
		childSpec: self;
		definitionsHolder: theNode definitionsHolder.
	aNode initEmptyChildren.
	theNode addChild: aNode.
	aNode updateNodeName.
	aNode initFilter.

	theNode addDependent: aNode toObject: theNode value.

	someChildrenValues	:= self childrenValuesForObject: theNode value.
	someChildrenValues := theNode filterChildrenValues: someChildrenValues.

	someChildrenValues isNil ifTrue: [ someChildrenValues := OrderedCollection new].

	someChildrenNodes	:= OrderedCollection new.

	someChildrenValues do: [:oneValue | 
		aChildNode := theNode preferredNodeClass new 
			browser: theNode browser; 
			value: oneValue;  
			displaySelector: self displaySelector;
			definitionsHolder: theNode definitionsHolder;
			childSpec: (theNode definitionsHolder metaSelectorsFor: oneValue).
		theNode addDependent: aChildNode toObject: oneValue.
		someChildrenNodes addLast: aChildNode].

	self recurseInheritedNode: aNode do: [:anObject :otherChildrenValues :aChildSpec| 
		theNode addDependent: theNode toObject: anObject.

		theNode addInheritanceValue: anObject forLinkSelector: aChildSpec inheritanceLinkSelector.

		otherChildrenValues do: [:oneValue | 
			aChildNode := theNode preferredNodeClass new 
				browser: theNode browser; 
				value: oneValue;   
				displaySelector: self displaySelector;
				definitionsHolder: theNode definitionsHolder;
				childSpec: (theNode definitionsHolder metaSelectorsFor: oneValue);
				isInherited: true.
			theNode addDependent: aChildNode toObject: oneValue.
			someChildrenNodes addLast: aChildNode]].


	someChildrenNodes do: [:oneNode |
		aNode addChild: oneNode.
		oneNode updateNodeName].
	^self!

childrenValuesForObject: theObject
			
	^self childrenValuesForObject: theObject ifNotUnderstood: [:anObject |
		DEBUGDvpt ifTrue: [
			Transcript show: 'Not understood : ',  anObject metaInfo name , '>', self metaInfo name ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		nil]!

childrenValuesForObject: theObject ifNotUnderstood: theBlock
			
	| someChildrenValues aFeatureMetaInfo anObjectMetaInfo |

	theObject isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil object in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'CollectionChildSpec without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	anObjectMetaInfo := theObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	(anObjectMetaInfo hasOrInheritsFeature: aFeatureMetaInfo ) ifFalse: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object type metaInfo does not include CollectionChildSpec feature metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := aFeatureMetaInfo getObjectFeatureValueTC: theObject.

	someChildrenValues  isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil result from ',  theObject class name , '>', self basicSelector ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := someChildrenValues select: [:otherValue | otherValue yourself isNil not].

	^someChildrenValues!

editRequiredFeaturesForNewObjectOfType: theFactoryType inNode: theNode
	
	| aFeatureMetaInfo aRequiredFeaturesTypeMetaInfo aRequiredFeaturesObject aDefinitionsHolder  aParentNode aParentObject aBrowserParameters aDiscardPerspectives aShowSeparateTextPerspectives |

	self isStatic ifTrue: [ ^nil].

	theFactoryType isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	aParentNode := (theNode isKindOf: METAObjectHolder) ifTrue: [ theNode owner] ifFalse: [ theNode parent].
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aRequiredFeaturesTypeMetaInfo := theFactoryType requiredFeaturesTypeMetaInfo.
	(aRequiredFeaturesTypeMetaInfo isNil or: [ aRequiredFeaturesTypeMetaInfo attributes isEmpty]) ifTrue: [  ^nil].

	aRequiredFeaturesObject := aRequiredFeaturesTypeMetaInfo createRequiredFeaturesObjectForParent: aParentObject
		fromFeatureMetaInfo: aFeatureMetaInfo.
	aRequiredFeaturesObject isNil ifTrue: [ ^nil].

	aDefinitionsHolder := theNode definitionsHolder.
	aDefinitionsHolder isNil ifTrue: [ ^nil].

	aBrowserParameters := theNode browserParameters.
	aBrowserParameters isNil ifTrue:  [
		 aBrowserParameters :=  Dictionary new: 13.
	].

	aDiscardPerspectives := aRequiredFeaturesTypeMetaInfo getDiscardPerspectivesInCreationDialogsParameterValue.
	aShowSeparateTextPerspectives := aRequiredFeaturesTypeMetaInfo getAlwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue.

	aBrowserParameters 
		at:  METABrowser showCanvasLabelParameterSymbol put: true;
		at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
		at:  METABrowser numberOfEditorHoldersParameterSymbol put: 1;
		at:  METABrowser initialVerticalCanvasesProportionParameterSymbol put: 0;
		at:  METABrowser discardPerspectivesParameterSymbol put: aDiscardPerspectives;
		at:  METABrowser discardTextPerspectivesParameterSymbol put: aShowSeparateTextPerspectives not.

	aDefinitionsHolder ensureHasViewForType: aRequiredFeaturesTypeMetaInfo.
	
	[ | aBrowserClass aDialogResult someIssues |
		aBrowserClass := aDefinitionsHolder preferredApplicationBrowserClass.
		aDialogResult := aBrowserClass
			openForObject: 			aRequiredFeaturesObject 
			definitionsHolder: 		aDefinitionsHolder
			browserParameters:	aBrowserParameters
			beDialog: true
			selectionOn: nil asValue.

		aDialogResult == true ifFalse: [ ^false].
	
		someIssues := aRequiredFeaturesTypeMetaInfo validateRequiredFeaturesObject: aRequiredFeaturesObject.
		(someIssues isNil or: [ someIssues isEmpty ])
			ifTrue: [ ^aRequiredFeaturesObject]
			ifFalse: [ 
				SimpleDialog new
					display: ('Por favor, revise la informacion introducida en los campos : ') withCRs
					fromList: (someIssues collect: [:anIssue |  anIssue first nlsName, ' : ', (anIssue at: 2) ]) 
					values: someIssues 
					lines: (((someIssues size + 1) max: 5) min: 18)
					cancel: [ ^false]
			]
	] repeat.!

moveDownObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveDownTC: aChildValue.!

moveToBottomObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveToBottomTC: aChildValue.!

moveToTopObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveToTopTC: aChildValue.!

moveUpObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveUpTC: aChildValue.!

removeObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aFeatureMetaInfo |

	self isStatic ifTrue: [ ^nil].

	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self error: 'Can not remove from an unknown collection'].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self error: 'Can not remove an unknown collection element value'].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  
		^self error: 'Can not remove without knowing about the collection node parent node'].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ 
		^self error: 'Can not remove without knowing about the collection node parent node value '].
	 
	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [ 
		^self error: 'Can not remove without knowing about the collection metainfo'].
 
	aFeatureMetaInfo object: aParentObject removeTC: aChildValue.! !

!CMOrderedCollectionChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMOrderedCollectionChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMPerspectiveSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMPerspectiveSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMTerminalChildSpec class publicMethodsFor: 'preferences'!

preferredCMAspectAdaptorWithCheckClass
	^self preferredPreferencesClass preferredCMAspectAdaptorWithCheckClass!

preferredCMEnumAspectAdaptorWithCheckClass
	^self preferredPreferencesClass preferredCMEnumAspectAdaptorWithCheckClass!

preferredPreferencesClass
	^CMPreferences! !

!CMTerminalChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMTerminalChildSpec publicMethodsFor: 'adaptors'!

buildAdaptor

	| anAdaptor aMetaInfo aValueType |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValueType := aMetaInfo valueType.
	aValueType isNil ifTrue: [ 
		^self preferredCMAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo
	].

	anAdaptor := aValueType isEnumeration
		ifTrue: [ self preferredCMEnumAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo]
		ifFalse: [ self preferredCMAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo].

	^anAdaptor!

buildCodeAdaptor

	| anAdaptor |
	anAdaptor := self preferredCMCodeAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.

	^anAdaptor!

buildIntervalAdaptors

	| anAdaptor aFirstAdaptor aLastAdaptor |

	anAdaptor := self preferredCMAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.
		
	aFirstAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #first
		assignWith: #first:.
	aFirstAdaptor subjectChannel: anAdaptor.
	aFirstAdaptor subjectSendsUpdates: true.

	aLastAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #last
		assignWith: #last:.
	aLastAdaptor subjectChannel: anAdaptor.
	aLastAdaptor subjectSendsUpdates: true.

	^Array with: anAdaptor with: aFirstAdaptor with: aLastAdaptor!

buildPointAdaptors

	| anAdaptor anXAdaptor anYAdaptor |
	anAdaptor := self preferredCMAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.
		
	anXAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #x
		assignWith: #x:.
	anXAdaptor subjectChannel: anAdaptor.
	anXAdaptor subjectSendsUpdates: true.

	anYAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #y
		assignWith: #y:.
	anYAdaptor subjectChannel: anAdaptor.
	anYAdaptor subjectSendsUpdates: true.

	^Array with: anAdaptor with: anXAdaptor with: anYAdaptor!

buildRectangleAdaptors

	| anAdaptor aLeftAdaptor aTopAdaptor aRightAdaptor aBottomAdaptor |
	anAdaptor := self preferredCMAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.
		
	aLeftAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #left
		assignWith: #moveToX:.
	aLeftAdaptor subjectChannel: anAdaptor.
	aLeftAdaptor subjectSendsUpdates: true.

	aTopAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #top
		assignWith: #moveToY:.
	aTopAdaptor subjectChannel: anAdaptor.
	aTopAdaptor subjectSendsUpdates: true.

	aRightAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #width
		assignWith: #width:.
	aRightAdaptor subjectChannel: anAdaptor.
	aRightAdaptor subjectSendsUpdates: true.

	aBottomAdaptor := self preferredSimpleDerivationAspectAdaptorWithCheckClass
		accessWith: #height
		assignWith: #height:.
	aBottomAdaptor subjectChannel: anAdaptor.
	aBottomAdaptor subjectSendsUpdates: true.

	^(Array with: anAdaptor), (Array with: aLeftAdaptor with: aTopAdaptor with: aRightAdaptor with: aBottomAdaptor)!

selectActionBlockOnAdaptor: theAdaptor

	^[ 
		(theAdaptor subjectChannel isNil not and: [
			theAdaptor subjectChannel value isNil not]) ifTrue: [
			self metaInfo getObjectFeatureValueTC: theAdaptor subjectChannel value ]
	]! !

!CMTerminalChildSpec publicMethodsFor: 'enums'!

buildEnumMenuWithSolver: theNLSSolver

	| someEnumValues aMenu someEnumLabels aFeatureMetaInfo aReferencedTypeMetaInfo someEnumAttributes |

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aReferencedTypeMetaInfo := aFeatureMetaInfo referencedType.
	aReferencedTypeMetaInfo isEnumeration ifFalse: [ ^nil].
 
	someEnumAttributes := aReferencedTypeMetaInfo allEnumerationAttributes.
	(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].
	someEnumLabels := someEnumAttributes collect: [:anAttribute |   anAttribute nlsName].
	someEnumValues := someEnumAttributes collect: [:anAttribute | anAttribute name].

	aMenu := RTDynamicHelpPopUpMenu
		labelList: (Array with: (Array with: 'Cancelar' )  with: someEnumLabels ) 
		values:	(Array with: nil) ,  someEnumValues.
	^aMenu! !

!CMTerminalChildSpec publicMethodsFor: 'interface construction'!

formatString

	| aMetaInfo aNonVirtualType aType aFormatString aPrimitiveBroker |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super formatString].
	
	aType := aMetaInfo referencedType.
	aType isNil ifTrue: [ ^super formatString].

	aNonVirtualType := aType nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^super formatString].

	aNonVirtualType isPrimitive ifFalse: [  ^super formatString].

	aPrimitiveBroker := aNonVirtualType primitiveBroker.
	aPrimitiveBroker isNil ifTrue: [ ^super formatString].

	aFormatString := aPrimitiveBroker formatString.
	aFormatString isNil ifTrue: [ ^super formatString].

	^aFormatString!

widgetType

	| aMetaInfo aNonVirtualType aType aWidgetType aPrimitiveBroker |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super widgetType].
	
	aType := aMetaInfo referencedType.
	aType isNil ifTrue: [ ^super widgetType].

	aNonVirtualType := aType nonVirtualType.
	aNonVirtualType isNil ifTrue: [ ^super widgetType].

	aNonVirtualType isPrimitive ifFalse: [  ^super widgetType].

	aPrimitiveBroker := aNonVirtualType primitiveBroker.
	aPrimitiveBroker isNil ifTrue: [ ^super widgetType].

	aWidgetType := aPrimitiveBroker widgetType.
	aWidgetType isNil ifTrue: [ ^super widgetType].

	^aWidgetType! !

!CMTerminalChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMTerminalChildSpec publicMethodsFor: 'nodes'!

nodeNameIncludes: theAspect
	| aMetaInfo |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^false].

	^aMetaInfo name = theAspect! !

!CMTerminalChildSpec publicMethodsFor: 'preferences'!

preferredCMAspectAdaptorWithCheckClass 
	^self class preferredCMAspectAdaptorWithCheckClass!

preferredCMEnumAspectAdaptorWithCheckClass 
	^self class preferredCMEnumAspectAdaptorWithCheckClass! !

!CMTerminalChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMTerminalChildSpec publicMethodsFor: 'value conversion'!

defaultFormatString
	^'mm/dd/yy' copy!

formatStringOrDefault
	| aFormatString |

	aFormatString := self formatString.

	(aFormatString isNil or: [ aFormatString isEmpty]) ifFalse:  [ ^aFormatString].

	^self defaultFormatString!

newPrintConverter
	| aPrintConverter |

	aPrintConverter := self preferredPrintConverterClass forChildSpec: self.

	aPrintConverter toPrintBlock: 		[:aValue | self printValue: aValue].
	aPrintConverter toFormatBlock: 		[:aValue | self printValue: aValue].
	aPrintConverter toReadBlock: 		[:aValue | self readValue: aValue].

	^aPrintConverter!

printValue: theValue

	| aMetaInfo aType |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].
	
	aType := aMetaInfo referencedType.
	aType isNil ifTrue: [ ^nil].

	^aType printValue: theValue!

readValue: theValue

	| aMetaInfo aType |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].
	
	aType := aMetaInfo referencedType.
	aType isNil ifTrue: [ ^nil].

	^aType readValue: theValue! !

!CMTerminalChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aValue anAttributeMetaInfo aTargetTypeMetaInfo aTargetAttributeMetaInfo |
	theTarget isNil ifTrue: [ ^nil].

	anAttributeMetaInfo := self metaInfo.
	anAttributeMetaInfo isNil ifTrue: [ ^nil].

	aTargetTypeMetaInfo := theTarget metaInfo.
	aTargetTypeMetaInfo isNil ifTrue: [ ^nil].
	
	aTargetAttributeMetaInfo := aTargetTypeMetaInfo effectiveFeatureNamed: anAttributeMetaInfo name.
	aTargetAttributeMetaInfo isNil ifTrue: [ ^nil].

	aValue := aTargetAttributeMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMTerminalCollectionChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMTerminalCollectionChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMTerminalCollectionChildSpec publicMethodsFor: 'filters'!

generateAutoFilterInDefinitionsHolder: theDefinitionsHolder
	^nil! !

!CMTerminalCollectionChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMTerminalCollectionChildSpec publicMethodsFor: 'nodes'!

addNewObjectInNode: theNode 
	
	| aParentNode aParentObject aFeatureMetaInfo aReferencedTypeMetaInfo aNewObject someEnumAttributes someEnumLabels someEnumValues aSelectedEnumValue |

	self isStatic ifTrue: [ ^nil].

	aParentNode := theNode parent.
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aReferencedTypeMetaInfo := aFeatureMetaInfo referencedType.
	aReferencedTypeMetaInfo isEnumeration 
		ifTrue: [  
			someEnumAttributes := aReferencedTypeMetaInfo allAttributes.
			(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].
			someEnumLabels := someEnumAttributes collect: [:anAttribute |   anAttribute nlsName].
			someEnumValues := someEnumAttributes collect: [:anAttribute | anAttribute name].

			aSelectedEnumValue := Dialog 
				choose: ('Please, select a ', self name) withCRs
				fromList: someEnumLabels 
				values: someEnumValues 
				lines: (((someEnumLabels size + 1) max: 5) min: 18)
				cancel: [nil].
			aSelectedEnumValue isNil ifTrue: [ ^nil].
			aNewObject := aReferencedTypeMetaInfo createEnumObject: aSelectedEnumValue.
		]
		ifFalse: [ 
			aNewObject := aReferencedTypeMetaInfo createObject.
		].

	aNewObject isNil ifTrue: [  ^nil].
					
	aFeatureMetaInfo object: aParentObject addTC: aNewObject.

	^aNewObject!

addNodesForNode: theNode 


	| aNode  someChildrenValues someChildrenNodes  aChildNode   aShowInherited someSortedChildrenNodes |

	aNode := (theNode preferredNodeClass new) 
		browser: theNode browser; 
		childSpec: self;
		definitionsHolder: theNode definitionsHolder.
	aNode initEmptyChildren.
	theNode addChild: aNode.
	aNode updateNodeName.
	
	theNode addDependent: aNode toObject: theNode value.

	someChildrenValues	:= self childrenValuesForObject: theNode value.
	someChildrenValues := theNode filterChildrenValues: someChildrenValues.

	someChildrenValues isNil ifTrue: [ someChildrenValues := OrderedCollection new].

	someChildrenNodes	:= OrderedCollection new.

	someChildrenValues do: [:oneValue | 
		aChildNode := theNode preferredNodeClass new 
			browser: theNode browser; 
			value: oneValue;  
			displaySelector: self displaySelector;
			definitionsHolder: theNode definitionsHolder;
			childSpec: self.
		theNode addDependent: aChildNode toObject: oneValue.
		someChildrenNodes addLast: aChildNode].

	self recurseInheritedNode: aNode do: [:anObject :otherChildrenValues :aChildSpec| 
		theNode addDependent: theNode toObject: anObject.

		theNode addInheritanceValue: anObject forLinkSelector: aChildSpec inheritanceLinkSelector.

		otherChildrenValues do: [:oneValue | 
			aChildNode := theNode preferredTerminalInCollectionNodeClass new 
				browser: theNode browser; 
				value: oneValue;   
				displaySelector: self displaySelector;
				definitionsHolder: theNode definitionsHolder;
				childSpec: self;
				isInherited: true.
			theNode addDependent: aChildNode toObject: oneValue.
			someChildrenNodes addLast: aChildNode]].

	aShowInherited :=  theNode browser isNil not and: [ theNode browser showInherited].

	someSortedChildrenNodes := self sortNodes: someChildrenNodes showInherited: aShowInherited.

	someSortedChildrenNodes do: [:oneNode |
		aNode addChild: oneNode.
		oneNode updateNodeName].
	^self!

childrenValuesForObject: theObject
			
	^self childrenValuesForObject: theObject ifNotUnderstood: [:anObject |
		DEBUGDvpt ifTrue: [
			Transcript show: 'Not understood : ',  anObject metaInfo name , '>', self metaInfo name ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		nil]!

childrenValuesForObject: theObject ifNotUnderstood: theBlock
			
	| someChildrenValues aFeatureMetaInfo anObjectMetaInfo |

	theObject isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil object in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'CollectionChildSpec without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	anObjectMetaInfo := theObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	(anObjectMetaInfo hasOrInheritsFeature: aFeatureMetaInfo ) ifFalse: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object type metaInfo does not include CollectionChildSpec feature metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := aFeatureMetaInfo getObjectFeatureValueTC: theObject.

	someChildrenValues  isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil result from ',  theObject class name , '>', self basicSelector ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := someChildrenValues select: [:otherValue | otherValue yourself isNil not].

	^someChildrenValues!

getNameForTerminalNode: theNode

	| aValue aName aMetaInfo |
	theNode isNil ifTrue: [ ^nil].

	aValue := theNode value.
	aValue isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^aValue printString].

	aName := aMetaInfo getTerminalObjectNameValue: aValue.
	^aName! !

!CMTerminalCollectionChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMTerminalCollectionChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMTerminalOrderedCollectionChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMTerminalOrderedCollectionChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMTerminalOrderedCollectionChildSpec publicMethodsFor: 'filters'!

generateAutoFilterInDefinitionsHolder: theDefinitionsHolder
	^nil! !

!CMTerminalOrderedCollectionChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMTerminalOrderedCollectionChildSpec publicMethodsFor: 'nodes'!

addNewObjectInNode: theNode 
	
	| aParentNode aParentObject aFeatureMetaInfo aReferencedTypeMetaInfo aNewObject someEnumAttributes someEnumLabels someEnumValues aSelectedEnumValue |

	self isStatic ifTrue: [ ^nil].

	aParentNode := theNode parent.
	aParentNode isNil ifTrue: [  ^nil].

	aParentObject  := aParentNode value.
	aParentObject isNil ifTrue: [  ^nil].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [  ^nil].
	
	aReferencedTypeMetaInfo := aFeatureMetaInfo referencedType.
	aReferencedTypeMetaInfo isEnumeration 
		ifTrue: [  
			someEnumAttributes := aReferencedTypeMetaInfo allEnumerationAttributes.
			(someEnumAttributes isNil or: [ someEnumAttributes isEmpty]) ifTrue: [ ^nil].
			someEnumLabels := someEnumAttributes collect: [:anAttribute |   anAttribute nlsName].
			someEnumValues := someEnumAttributes collect: [:anAttribute | anAttribute name].

			aSelectedEnumValue := Dialog 
				choose: ('Please, select a ', self name) withCRs
				fromList: someEnumLabels 
				values: someEnumValues 
				lines: (((someEnumLabels size + 1) max: 5) min: 18)
				cancel: [nil].
			aSelectedEnumValue isNil ifTrue: [ ^nil].
			aNewObject := aReferencedTypeMetaInfo createEnumObject: aSelectedEnumValue.
		]
		ifFalse: [ 
			aNewObject := aReferencedTypeMetaInfo createObject.
		].

	aNewObject isNil ifTrue: [  ^nil].
					
	aFeatureMetaInfo object: aParentObject addTC: aNewObject.

	^aNewObject!

addNodesForNode: theNode 


	| aNode  someChildrenValues someChildrenNodes  aChildNode   |

	aNode := theNode preferredTerminalCollectionNodeClass new 
		browser: theNode browser; 
		childSpec: self;
		definitionsHolder: theNode definitionsHolder.
	aNode initEmptyChildren.
	theNode addChild: aNode.
	aNode updateNodeName.

	theNode addDependent: aNode toObject: theNode value.

	someChildrenValues	:= self childrenValuesForObject: theNode value.
	someChildrenValues := theNode filterChildrenValues: someChildrenValues.

	someChildrenValues isNil ifTrue: [ someChildrenValues := OrderedCollection new].

	someChildrenNodes	:= OrderedCollection new.

	someChildrenValues do: [:oneValue | 
		aChildNode := theNode preferredNodeClass new 
			browser: theNode browser; 
			value: oneValue;  
			displaySelector: self displaySelector;
			definitionsHolder: theNode definitionsHolder;
			childSpec: self.
		theNode addDependent: aChildNode toObject: oneValue.
		someChildrenNodes addLast: aChildNode].

	self recurseInheritedNode: aNode do: [:anObject :otherChildrenValues :aChildSpec| 
		theNode addDependent: theNode toObject: anObject.

		theNode addInheritanceValue: anObject forLinkSelector: aChildSpec inheritanceLinkSelector.

		otherChildrenValues do: [:oneValue | 
			aChildNode := theNode preferredTerminalInCollectionNodeClass new 
				browser: theNode browser; 
				value: oneValue;   
				displaySelector: self displaySelector;
				definitionsHolder: theNode definitionsHolder;
				childSpec: self;
				isInherited: true.
			theNode addDependent: aChildNode toObject: oneValue.
			someChildrenNodes addLast: aChildNode]].

	someChildrenNodes do: [:oneNode |
		aNode addChild: oneNode.
		oneNode updateNodeName].
	^self!

childrenValuesForObject: theObject
			
	^self childrenValuesForObject: theObject ifNotUnderstood: [:anObject |
		DEBUGDvpt ifTrue: [
			Transcript show: 'Not understood : ',  anObject metaInfo name , '>', self metaInfo name ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		nil]!

childrenValuesForObject: theObject ifNotUnderstood: theBlock
			
	| someChildrenValues aFeatureMetaInfo anObjectMetaInfo |

	theObject isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil object in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'CollectionChildSpec without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	anObjectMetaInfo := theObject metaInfo.
	anObjectMetaInfo isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object without metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	(anObjectMetaInfo hasOrInheritsFeature: aFeatureMetaInfo ) ifFalse: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Object type metaInfo does not include CollectionChildSpec feature metaInfo in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := aFeatureMetaInfo getObjectFeatureValueTC: theObject.

	someChildrenValues  isNil ifTrue: [
		DEBUGDvpt ifTrue: [
			Transcript show: 'Nil result from ',  theObject class name , '>', self basicSelector ,  
				' in ', 
				thisContext sender homeReceiver class name ,'>',  thisContext sender selector; cr].
		^#() copy].

	someChildrenValues := someChildrenValues select: [:otherValue | otherValue yourself isNil not].

	^someChildrenValues!

getNameForTerminalNode: theNode

	| aValue aName aMetaInfo aValueTypeMetaInfo |
	theNode isNil ifTrue: [ ^nil].

	aValue := theNode value.
	aValue isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^aValue printString].

	aValueTypeMetaInfo := aMetaInfo valueType.
	aValueTypeMetaInfo isNil ifTrue: [ ^aValue printString].
	
	aName := aValueTypeMetaInfo getTerminalObjectNameValue: aValue.
	^aName!

moveDownObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveDownTC: aChildValue.!

moveToBottomObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveToBottomTC: aChildValue.!

moveToTopObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveToTopTC: aChildValue.!

moveUpObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aMetaInfo |
	self isStatic ifTrue: [ ^self].
	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  ^self].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ ^self].
	 
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aMetaInfo object: aParentObject moveUpTC: aChildValue.!

removeObjectInNode: theChildNode

	| aParentNode aParentObject    aChildValue aNode aFeatureMetaInfo |

	self isStatic ifTrue: [ ^nil].

	theChildNode isNil ifTrue: [ ^self].

	aNode := theChildNode parent.
	aNode isNil ifTrue: [  ^self error: 'Can not remove from an unknown collection'].

	aChildValue := theChildNode value.
	aChildValue isNil ifTrue: [  ^self error: 'Can not remove an unknown collection element value'].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [  
		^self error: 'Can not remove without knowing about the collection node parent node'].

	aParentObject := aParentNode value.
	aParentObject isNil ifTrue: [ 
		^self error: 'Can not remove without knowing about the collection node parent node value '].
	 
	aFeatureMetaInfo := self metaInfo.
	aFeatureMetaInfo isNil ifTrue: [ 
		^self error: 'Can not remove without knowing about the collection metainfo'].
 
	aFeatureMetaInfo object: aParentObject removeTC: aChildValue.! !

!CMTerminalOrderedCollectionChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMTerminalOrderedCollectionChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMTextChildSpec class publicMethodsFor: 'preferences'!

preferredCMAspectAdaptorWithCheckClass
	^self preferredPreferencesClass preferredCMAspectAdaptorWithCheckClass!

preferredPreferencesClass
	^CMPreferences! !

!CMTextChildSpec publicMethodsFor: 'accessing'!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMTextChildSpec publicMethodsFor: 'adaptors'!

buildAdaptor

	| aMetaInfo anAdaptor |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	anAdaptor := self preferredCMAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.
	^anAdaptor!

buildCodeAdaptor

	| anAdaptor |
	anAdaptor := self preferredCMCodeAspectAdaptorWithCheckClass newWithMetaInfo: self metaInfo.

	^anAdaptor!

selectActionBlockOnAdaptor: theAdaptor

	^[ 
		(theAdaptor subjectChannel isNil not and: [
			theAdaptor subjectChannel value isNil not]) ifTrue: [
			self metaInfo getObjectFeatureValueTC: theAdaptor subjectChannel value ]
	]! !

!CMTextChildSpec publicMethodsFor: 'nls'!

nlsName
	
	| aMetaInfo |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^super nlsName].

	^aMetaInfo nlsName! !

!CMTextChildSpec publicMethodsFor: 'nodes'!

nodeNameIncludes: theAspect
	| aMetaInfo |
	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^false].

	^aMetaInfo name = theAspect! !

!CMTextChildSpec publicMethodsFor: 'preferences'!

preferredCMAspectAdaptorWithCheckClass 
	^self class preferredCMAspectAdaptorWithCheckClass! !

!CMTextChildSpec publicMethodsFor: 'testing'!

isCMDriven
	^true! !

!CMTextChildSpec publicMethodsFor: 'values'!

getValueFor: theTarget
	| aMetaInfo aValue |
	theTarget isNil ifTrue: [ ^nil].

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aValue := aMetaInfo getObjectFeatureValueTC: theTarget.
	^aValue! !

!CMTreeChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

!CMTreeEditChildSpec class publicMethodsFor: 'preferences'!

preferredPreferencesClass
	^CMPreferences! !

CMClassChildSpec initializeAfterLoad!
CMCollectionChildSpec initializeAfterLoad!
CMOrderedCollectionChildSpec initializeAfterLoad!
CMTerminalCollectionChildSpec initializeAfterLoad!
CMTerminalOrderedCollectionChildSpec initializeAfterLoad!
CMOperationVoidNoArgsChildSpec initializeAfterLoad!
CMTerminalChildSpec initializeAfterLoad!
CMDiagramChildSpec initializeAfterLoad!
CMImageChildSpec initializeAfterLoad!
CMTextChildSpec initializeAfterLoad!
CMCodeChildSpec initializeAfterLoad!
CMHyperTextChildSpec initializeAfterLoad!
CMTreeChildSpec initializeAfterLoad!
CMTreeEditChildSpec initializeAfterLoad!
CMPerspectiveSpec initializeAfterLoad!
CODE_META_Specs initializeAfterLoad!

CODE_META_Specs loaded!
