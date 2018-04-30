'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



Application create: #CODEgenUI with: 
    (#( META)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

CODEgenUI becomeDefault!

METADefinitionsHolder subclass: #CODEMModelDefinitionsHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

METAEditorsOpener subclass: #CODEMModelEditorsOpener
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

METAConfiguration subclass: #CODEMModelMETAConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

METAConfigurationsCollection subclass: #CODEMModelConfigurationsCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

METAPathFinderScopedApplicationBrowser subclass: #CODEMModelPathFinderGenericBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

CODEMModelPathFinderGenericBrowser subclass: #CODEMModelChooser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

METAConfigurationsBrowser subclass: #CODEMModelConfigurationsBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

METAScopedApplicationBrowser subclass: #CODEMModelGenericBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

Application subclass: #CODEgenUI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODEgenUI becomeDefault!

!CODEgenUI class publicMethodsFor: 'ENVY'!

loaded
	CODEMModelEditorsOpener initialize.
	CODEMModelMETAConfiguration initialize.
	CODEMModelConfigurationsCollection initialize.
	CODEMModelPathFinderGenericBrowser initialize.
	CODEMModelConfigurationsBrowser initialize.! !

!CODEMModelChooser class publicMethodsFor: 'class initialization'!

initialize
	"CODEMModelChooser initialize.
	CODEMModelChooser allSubclasses do: [:aClass | aClass initialize]"


	super initialize! !

!CODEMModelChooser class publicMethodsFor: 'configuration'!

numberOfChooserEditorHoldersConfigParmValue

	^self getMETAConfigurationParameterValue: #numberOfChooserEditorHolders default: 3! !

!CODEMModelChooser class publicMethodsFor: 'instance creation'!

browserKind
	^#CODEMModelChooser!

checkedBrowserParameters:	theBrowserParameters
	| aDict |

	aDict := theBrowserParameters isNil ifTrue: [ Dictionary new] ifFalse: [ theBrowserParameters].

	self in: aDict at: METABrowser showCanvasLabelParameterSymbol 			ifAbsentPut: true.
	self in: aDict at: METABrowser editorsOpenerParameterSymbol 				ifAbsentPut: CODEMModelEditorsOpener editorsOpener.
	self in: aDict at: METABrowser numberOfEditorHoldersParameterSymbol 	ifAbsentPut: self defaultNumberOfEditorHolders.
	self in: aDict at:  METABrowser initialVerticalCanvasesProportionParameterSymbol ifAbsentPut: 100.
	
	^super checkedBrowserParameters: theBrowserParameters.!

defaultNumberOfEditorHolders
	^self numberOfChooserEditorHoldersConfigParmValue! !

!CODEMModelConfigurationsBrowser class publicMethodsFor: 'class initialization'!

initialize
	"CODEMModelConfigurationsBrowser initialize.
	CODEMModelConfigurationsBrowser allSubclasses do: [:aClass | aClass initialize]"

	super initialize
"*VIPVersion 25-6-96 | 2:47:35 pm 'Anonymous'*"! !

!CODEMModelConfigurationsBrowser class publicMethodsFor: 'ref:class accessing'!

browserKind
	^#CODEMModelConfigurationsBrowser! !

!CODEMModelConfigurationsBrowser class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsCollectionClass
	^CODEMModelConfigurationsCollection!

preferredEditorsOpenerClass
	^METAEditorsOpener! !

!CODEMModelConfigurationsBrowser publicMethodsFor: 'ref:updating'!

windowLabelString

	^super windowLabelString
"*VIPVersion 25-6-96 | 2:56:54 pm 'Anonymous'*"! !

!CODEMModelConfigurationsCollection class publicMethodsFor: 'class initialization'!

initialize
	"CODEMModelConfigurationsCollection initialize"

	super initialize! !

!CODEMModelConfigurationsCollection class publicMethodsFor: 'ref:configurations'!

allCurrentConfigurations
	^Array with: CODEMModelMETAConfiguration current! !

!CODEMModelDefinitionsHolder class publicMethodsFor: 'instance creation'!

forMapEditorSelectingFromSourceModel

	|     aDefinitionsHolder    someViews somePaths somePerspectives someVPP |
	
	someVPP := self viewsPerspectivesPathsForMapEditorSelectingFromSourceModel.

	someViews 			:= 	someVPP at: 1.
	somePerspectives	:= 	someVPP at: 2.
	somePaths 			:= 	someVPP at: 3.

	aDefinitionsHolder := self 
		views: 							someViews
		applicationModels: 				nil
		listMenus:						nil
		styleModifiers:					nil
		perspectives:					somePerspectives
		perspectivesApplicationModels:	nil
		paths:								somePaths
		filters:								nil.

	^aDefinitionsHolder!

forModelEditor
	|     aDefinitionsHolder    someViews somePaths somePerspectives someVPP |
	
	someVPP := self viewsPerspectivesPathsForModelEditor.

	someViews 			:= 	someVPP at: 1.
	somePerspectives	:= 	someVPP at: 2.
	somePaths 			:= 	someVPP at: 3.

	aDefinitionsHolder := self 
		views: 							someViews
		applicationModels: 				nil
		listMenus:						nil
		styleModifiers:					nil
		perspectives:					somePerspectives
		perspectivesApplicationModels:	nil
		paths:								somePaths
		filters:								nil.

	^aDefinitionsHolder!

viewsPerspectivesPathsForMapEditorSelectingFromSourceModel
	|     someViews somePaths somePerspectives someVPP someModelViews someModelPerspectives someModelPaths someMapViews someMapPerspectives someMapPaths |

	someViews 			:= 	Dictionary new.
	somePerspectives 	:= 	Dictionary new.
	somePaths 			:= 	Dictionary new.

	someVPP := self viewsPerspectivesPathsModelForModelEditor.

	someModelViews 			:= 	someVPP at: 1.
	someModelViews associationsDo: [:anAssoc | someViews at: anAssoc key put: anAssoc value].

	someModelPerspectives	:= 	someVPP at: 2.
	someModelPerspectives associationsDo: [:anAssoc | somePerspectives at: anAssoc key put: anAssoc value].

	someModelPaths 			:= 	someVPP at: 3.
	someModelPaths associationsDo: [:anAssoc | somePaths at: anAssoc key put: anAssoc value].

	someVPP := self viewsPerspectivesPathsMapForMapEditorSelectingFromSourceModel.

	someMapViews 			:= 	someVPP at: 1.
	someMapViews associationsDo: [:anAssoc | someViews at: anAssoc key put: anAssoc value].

	someMapPerspectives	:= 	someVPP at: 2.
	someMapPerspectives associationsDo: [:anAssoc | somePerspectives at: anAssoc key put: anAssoc value].

	someMapPaths 			:= 	someVPP at: 3.
	someMapPaths associationsDo: [:anAssoc | somePaths at: anAssoc key put: anAssoc value].

	^Array with: someViews with: somePerspectives with: somePaths!

viewsPerspectivesPathsForModelEditor
	|     someViews somePaths somePerspectives someVPP someModelViews someModelPerspectives someModelPaths someMapViews someMapPerspectives someMapPaths someVPPMap |

	someViews 			:= 	Dictionary new.
	somePerspectives 	:= 	Dictionary new.
	somePaths 			:= 	Dictionary new.

	someVPP := self viewsPerspectivesPathsModelForModelEditor.

	someModelViews 			:= 	someVPP at: 1.
	someModelViews associationsDo: [:anAssoc | someViews at: anAssoc key put: anAssoc value].

	someModelPerspectives	:= 	someVPP at: 2.
	someModelPerspectives associationsDo: [:anAssoc | somePerspectives at: anAssoc key put: anAssoc value].

	someModelPaths 			:= 	someVPP at: 3.
	someModelPaths associationsDo: [:anAssoc | somePaths at: anAssoc key put: anAssoc value].

	someVPPMap := self viewsPerspectivesPathsMapForModelEditor.

	someMapViews 			:= 	someVPPMap at: 1.
	someMapViews associationsDo: [:anAssoc | someViews at: anAssoc key put: anAssoc value].

	someMapPerspectives	:= 	someVPPMap at: 2.
	someMapPerspectives associationsDo: [:anAssoc | somePerspectives at: anAssoc key put: anAssoc value].

	someMapPaths 			:= 	someVPPMap at: 3.
	someMapPaths associationsDo: [:anAssoc | somePaths at: anAssoc key put: anAssoc value].

	^Array with: someViews with: somePerspectives with: somePaths!

viewsPerspectivesPathsMapForMapEditorSelectingFromSourceModel
	|     someViews somePaths somePerspectives |

	someViews := 	Dictionary new
		at: CODEMap put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEMap;
			metaSelectorsSelector: #mapEditorSelectingFromSourceModelMETASelectors);
		at: CODEMapsFolder put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEMapsFolder;
			metaSelectorsSelector: #mapEditorSelectingFromSourceModelMETASelectors);
		at: CODEElementMap put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEElementMap;
			metaSelectorsSelector: #mapEditorSelectingFromSourceModelMETASelectors);
		at: CODEMapLogic put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEMapLogic;
			metaSelectorsSelector: #mapEditorSelectingFromSourceModelMETASelectors);

		yourself.


 	somePaths := Dictionary new
		at: CODEMap put: CODEMap mapEditorSelectingFromSourceModelPathSelectors;
		at: CODEMapsFolder put: CODEMapsFolder mapEditorSelectingFromSourceModelPathSelectors;
		at: CODEElementMap put: CODEElementMap mapEditorSelectingFromSourceModelPathSelectors;
		at: CODEMapLogic put: CODEMapLogic mapEditorSelectingFromSourceModelPathSelectors;
		yourself.
		

	somePerspectives := Dictionary new
		at: CODEMap put: CODEMap mapEditorSelectingFromSourceModelMETAPerspectives;
		at: CODEMapsFolder put: CODEMapsFolder mapEditorSelectingFromSourceModelMETAPerspectives;
		at: CODEElementMap put: CODEElementMap mapEditorSelectingFromSourceModelMETAPerspectives;
		at: CODEMapLogic put: CODEMapLogic mapEditorSelectingFromSourceModelMETAPerspectives;
		yourself.
		

	^Array with: someViews with: somePerspectives with: somePaths!

viewsPerspectivesPathsMapForModelEditor
	|     someViews somePaths somePerspectives |

	someViews := 	Dictionary new
		at: CODEMap put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEMap;
			metaSelectorsSelector: #modelEditorMETASelectors);
		at: CODEMapsFolder put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEMapsFolder;
			metaSelectorsSelector: #modelEditorMETASelectors);
		at: CODEElementMap put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEElementMap;
			metaSelectorsSelector: #modelEditorMETASelectors);
		at: CODEMapLogic put: (self preferredCacheEntryClass new
			metaSelectorsSource: CODEMapLogic;
			metaSelectorsSelector: #modelEditorMETASelectors);

		yourself.


 	somePaths := Dictionary new
		at: CODEMap put: CODEMap modelEditorPathSelectors;
		at: CODEMapsFolder put: CODEMapsFolder modelEditorPathSelectors;
		at: CODEElementMap put: CODEElementMap modelEditorPathSelectors;
		at: CODEMapLogic put: CODEMapLogic modelEditorPathSelectors;
		yourself.
		

	somePerspectives := Dictionary new
		at: CODEMap put: CODEMap modelEditorMETAPerspectives;
		at: CODEMapsFolder put: CODEMapsFolder modelEditorMETAPerspectives;
		at: CODEElementMap put: CODEElementMap modelEditorMETAPerspectives;
		at: CODEMapLogic put: CODEMapLogic modelEditorMETAPerspectives;
		yourself.
		

	^Array with: someViews with: somePerspectives with: somePaths!

viewsPerspectivesPathsModelForModelEditor
	|     someViews somePaths somePerspectives someTopClasses someModelClassNames someModelClasses |

	someViews := 	Dictionary new: 133.
	somePaths := Dictionary new: 133.
	somePerspectives := Dictionary new: 133.

	someModelClassNames := #(CODEModel CODEModule CODEType CODEAttribute CODEAttributeRefinement CODERelationship CODERelationshipRefinement  CODEComment  CODEParameter CODEAspect  CODEOperation CODEOperationRefinement CODEArgument CODEArgumentRefinement CMTypeDependency CMTypeObserver).
		
	someModelClasses := someModelClassNames collect: [:aClassName | Smalltalk at: aClassName].

	someModelClasses do: [:aClass |

			someViews at: aClass put: (self preferredCacheEntryClass new
				metaSelectorsSource: aClass;
				metaSelectorsSelector: #modelEditorMETASelectors).

			somePaths at: aClass put: aClass modelEditorPathSelectors.

			somePerspectives at: aClass put: aClass modelEditorMETAPerspectives
	].

	someTopClasses := (Array with: CMViewCollaborator with: CMAbstractNode).
	someTopClasses do: [:aTopClass |

		aTopClass withAllSubclassesDo: [:aClass |
			someViews at: aClass put: (self preferredCacheEntryClass new
				metaSelectorsSource: aClass;
				metaSelectorsSelector: #modelEditorMETASelectors)
		].

		aTopClass withAllSubclassesDo: [:aClass |
			somePaths at: aClass put: aClass modelEditorPathSelectors
		].

		aTopClass withAllSubclassesDo: [:aClass |
			somePerspectives at: aClass put: aClass modelEditorMETAPerspectives
		].
	].

	^Array with: someViews with: somePerspectives with: somePaths! !

!CODEMModelEditorsOpener class publicMethodsFor: 'class accessing'!

editorsOpenerName
	^#CODEMModelGlobalEditorsOpener! !

!CODEMModelEditorsOpener class publicMethodsFor: 'class initialization'!

initialize
	"CODEMModelEditorsOpener initialize.
	CODEMModelEditorsOpener allSubclasses do: [:aClass | aClass initialize]"

	self installGlobalEditorsOpener! !

!CODEMModelGenericBrowser class publicMethodsFor: 'class initialization'!

initialize
	"CODEMModelGenericBrowser initialize.
	CODEMModelGenericBrowser allSubclasses do: [:aClass | aClass initialize]"


	super initialize! !

!CODEMModelGenericBrowser class publicMethodsFor: 'instance creation'!

browserKind
	^#CODEMModelGenericBrowser!

checkedBrowserParameters:	theBrowserParameters
	| aDict |

	aDict := theBrowserParameters isNil ifTrue: [ Dictionary new] ifFalse: [ theBrowserParameters].

	self in: aDict at: METABrowser showCanvasLabelParameterSymbol 			ifAbsentPut: true.
	self in: aDict at: METABrowser editorsOpenerParameterSymbol 				ifAbsentPut: CODEMModelEditorsOpener editorsOpener.
	
	^super checkedBrowserParameters:	theBrowserParameters.!

defaultNumberOfEditorHolders
	^nil
	"delegate on configuration values"! !

!CODEMModelGenericBrowser class publicMethodsFor: 'interface specs'!

windowSpec

	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min: #(#Point 400 300 ) #bounds: #(#Rectangle 32 32 932 732 )  #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!CODEMModelGenericBrowser class publicMethodsFor: 'ref-preferences'!

preferredConfigurationsBrowserClass
	^CODEMModelConfigurationsBrowser!

preferredEditorsOpenerClass
	^CODEMModelEditorsOpener!

preferredMETAConfigurationClass

	^CODEMModelMETAConfiguration! !

!CODEMModelGenericBrowser publicMethodsFor: 'accessing'!

numberOfEditorHolders
	| aBrowserParameters aNumEditorHolders anObjectHolder |

	anObjectHolder := self objectHolder.
	anObjectHolder isNil ifFalse: [ 
		aBrowserParameters := anObjectHolder browserParameters.
		aBrowserParameters isNil ifFalse: [ 
			aNumEditorHolders := aBrowserParameters at: METABrowser numberOfEditorHoldersParameterSymbol ifAbsent: [ nil].
			aNumEditorHolders isNil ifFalse: [ ^aNumEditorHolders]]].


	 ^super numberOfEditorHolders "delegate on default way of finding it : through class method on configuration"! !

!CODEMModelGenericBrowser publicMethodsFor: 'label'!

browserLabelTitlePrefix
	^'MModel Browser on ' copy! !

!CODEMModelMETAConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"CODEMModelMETAConfiguration initialize"

	super initialize! !

!CODEMModelMETAConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^('Browser Configuration.\', 
		'Use parameters in this Configuration to control presentation of MModel navigators') copy withCRs!

configurationName
	^'MModel navigator' copy! !

!CODEMModelMETAConfiguration class publicMethodsFor: 'ref:parametersvalues'!

numberOfChooserEditorHoldersParameterValue
	^3!

numberOfEditorHoldersParameterValue
	^6!

preferredConfigurationsCollectionClass
	^CODEMModelConfigurationsCollection! !

!CODEMModelPathFinderGenericBrowser class publicMethodsFor: 'class initialization'!

initialize
	"CODEMModelPathFinderGenericBrowser initialize.
	CODEMModelPathFinderGenericBrowser allSubclasses do: [:aClass | aClass initialize]"


	super initialize! !

!CODEMModelPathFinderGenericBrowser class publicMethodsFor: 'instance creation'!

browserKind
	^#CODEMModelPathFinderGenericBrowser!

checkedBrowserParameters:	theBrowserParameters
	| aDict |

	aDict := theBrowserParameters isNil ifTrue: [ Dictionary new] ifFalse: [ theBrowserParameters].

	self in: aDict at: METABrowser showCanvasLabelParameterSymbol 			ifAbsentPut: true.
	self in: aDict at: METABrowser editorsOpenerParameterSymbol 				ifAbsentPut: CODEMModelEditorsOpener editorsOpener.
	self in: aDict at: METABrowser numberOfEditorHoldersParameterSymbol 	ifAbsentPut: self defaultNumberOfEditorHolders.
	
	^super checkedBrowserParameters:	theBrowserParameters.!

defaultNumberOfEditorHolders
	^nil
	"delegate on configuration values"! !

!CODEMModelPathFinderGenericBrowser class publicMethodsFor: 'interface specs'!

aboutSpec
	"UIPainter new openOnClass: self andSelector: #aboutSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'About...' #min: #(#Point 387 270 ) #max: #(#Point 387 270 ) #bounds: #(#Rectangle 6 200 393 470 ) ) #component: #(#SpecCollection #collection: #(#(#ActionButtonSpec #layout: #(#Rectangle 300 240 380 264 ) #model: #accept #label: 'OK' #isDefault: true #defaultable: true ) #(#InputFieldSpec #layout: #(#Rectangle 12 11 356 31 ) #flags: 0 #model: #versionText #tabable: false #style: #small #isReadOnly: true #type: #text ) #(#TextEditorSpec #layout: #(#Rectangle 7 41 375 227 ) #flags: 4 #model: #aboutText #style: #small #isReadOnly: true ) ) ) )
"*VIPVersion 2-12-95 | 9:55:24 pm 'ACV'*"!

technicalSupportSpec
	"UIPainter new openOnClass: self andSelector: #technicalSupportSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Autor y soporte KronoWare' 
			#min: #(#Point 461 286 ) 
			#max: #(#Point 461 286 ) 
			#bounds: #(#Rectangle 386 475 847 761 ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#LabelSpec 
					#layout: #(#Point 14 193 ) 
					#label: 'Telefono en Europa' ) 
				#(#LabelSpec 
					#layout: #(#Point 14 223 ) 
					#label: 'Telefono en EEUU' ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin 16 0.5 12 0 0.5 0 ) 
					#label: 'Soporte KRONOware' ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin -18 1 193 0 1 0 ) 
					#label: '+34 96 383 6999' ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin -18 1 223 0 1 0 ) 
					#label: '+1 305 252 9681' ) 
				#(#LabelSpec 
					#layout: #(#Point 14 253 ) 
					#label: 'Correo electronico' ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin -18 1 253 0 1 0 ) 
					#label: 'carrascv@telocity.com' ) 
				#(#LabelSpec 
					#layout: #(#Point 14 283 ) 
					#label: 'Pagina Web' ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin -18 1 283 0 1 0 ) 
					#label: 'www.dosmil-e.com' ) 
				#(#LabelSpec 
					#layout: #(#Point 14 50 ) 
					#label: 'KRONOware es propiedad intelectual e' ) 
				#(#LabelSpec 
					#layout: #(#Point 14 80 ) 
					#label: 'industrial exclusiva de su Autor' ) 
				#(#LabelSpec 
					#layout: #(#Point 14 110 ) 
					#label: 'Antonio Carrasco Valero DNINIF 24.334.236' ) 
				#(#LabelSpec 
					#layout: #(#Point 14 140 ) 
					#label: 'Todos los derechos reservados' ) 
				#(#ActionButtonSpec 
					#layout: #(#AlignmentOrigin 0 0.5 340 0 0.5 0 ) 
					#model: #cancel 
					#label: 'OK' 
					#defaultable: true ) ) ) )!

windowSpec


	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min: #(#Point 400 300 ) #bounds: #(#Rectangle 32 32 932 732 ) #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!CODEMModelPathFinderGenericBrowser class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsBrowserClass
	^CODEMModelConfigurationsBrowser!

preferredEditorsOpenerClass
	^CODEMModelEditorsOpener!

preferredMETAConfigurationClass

	^CODEMModelMETAConfiguration! !

!CODEMModelPathFinderGenericBrowser publicMethodsFor: 'accessing'!

numberOfEditorHolders
	| aBrowserParameters aNumEditorHolders anObjectHolder |

	anObjectHolder := self objectHolder.
	anObjectHolder isNil ifFalse: [ 
		aBrowserParameters := anObjectHolder browserParameters.
		aBrowserParameters isNil ifFalse: [ 
			aNumEditorHolders := aBrowserParameters at: METABrowser numberOfEditorHoldersParameterSymbol ifAbsent: [ nil].
			aNumEditorHolders isNil ifFalse: [ ^aNumEditorHolders]]].


	 ^super numberOfEditorHolders "delegate on default way of finding it : through class method on configuration"! !

!CODEMModelPathFinderGenericBrowser publicMethodsFor: 'label'!

browserLabelTitlePrefix
	^'MModel Browser on ' copy! !

CODEMModelDefinitionsHolder initializeAfterLoad!
CODEMModelEditorsOpener initializeAfterLoad!
CODEMModelMETAConfiguration initializeAfterLoad!
CODEMModelConfigurationsCollection initializeAfterLoad!
CODEMModelPathFinderGenericBrowser initializeAfterLoad!
CODEMModelChooser initializeAfterLoad!
CODEMModelConfigurationsBrowser initializeAfterLoad!
CODEMModelGenericBrowser initializeAfterLoad!
CODEgenUI initializeAfterLoad!

CODEgenUI loaded!
