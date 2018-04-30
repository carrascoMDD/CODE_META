'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_DefinedUI in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_DefinedUI becomeDefault!

Object subclass: #CMDefinedPart
	instanceVariableNames: 'name comment visibilityPredicate initiallyVisiblePredicate enablementPredicate initiallyEnabledPredicate container dirty timestamp '
	classVariableNames: 'CMDefinedPartKindsFactoryCache MenuCMDefinedPartKinds '
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPart subclass: #CMDefinedPartWithLayoutRectangle
	instanceVariableNames: 'layoutSymbol layoutGroup layoutRectangle layoutMode layoutFrame '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartAnonymous
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartCode
	instanceVariableNames: 'aspect change menu readOnly initialSelection '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartCode subclass: #CMDefinedPartLongField
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartColumn
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartField
	instanceVariableNames: 'aspect change metaInfo valueExpression menu initialSelection enhance justification borderWidth metaInfoRefTmpValues '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartGroup
	instanceVariableNames: 'bordered subParts multiplexed aspect layoutRectangles storeMethodSelector storeClassName '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartImage
	instanceVariableNames: 'imageSelector borderWidth '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartList
	instanceVariableNames: 'printItems oneItem aspect change list menu initialSelection useIndex useScrollBar textStyle '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartList subclass: #CMDefinedPartMultiList
	instanceVariableNames: 'columnsLayoutGroup columnsLayoutRectangles columns '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartSep
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartSlider
	instanceVariableNames: 'aspect change estilo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartSlider subclass: #CMDefinedPartXYSlider
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartTranscript
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithLayoutRectangle subclass: #CMDefinedPartWithNLS
	instanceVariableNames: 'nlsSymbol nlsGroup defaultTranslation '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithNLS subclass: #CMDefinedPartButton
	instanceVariableNames: 'actionSelector metaInfo actionExpression metaInfoRefTmpValues '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithNLS subclass: #CMDefinedPartSwitch
	instanceVariableNames: 'getSelector putSelector selectValue metaInfo valueExpression metaInfoRefTmpValues '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPartWithNLS subclass: #CMDefinedPartTitle
	instanceVariableNames: 'enhance justification borderWidth metaInfo metaInfoRefTmpValues '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

CMDefinedPart subclass: #CMDefinedWindow
	instanceVariableNames: 'titleNlsSymbol titleNlsGroup titleDefaultTranslation origin prefExtent minExtent maxExtent parts layoutRectangles storeMethodSelector storeClassName model allowedRootTypes allowedSubTypes allowMode allowExpression modelRefTmpValues allowedRootTypesRefsTmpValues allowedSubTypesRefsTmpValues nlsAppName defaultApplicationTranslationStoreClassName defaultApplicationTranslationStoreMethodSelector '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

Object subclass: #CMDefinedPartsInstaller
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

METATemplateCustomAppModel subclass: #CMDefinedPanelAppModel
	instanceVariableNames: 'definedWindow adaptorsCache layoutsCacheDictionary '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

SubApplication subclass: #CODE_META_DefinedUI
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI becomeDefault!

!CMDefinedPanelAppModel class publicMethodsFor: 'cmDefinedParts persistence'!

exampleDefinedWindowStore

	"(CMDefinedPart newFromPersistenceAsCode: CMDefinedPanelAppModel exampleDefinedWindowStore) browse"

	self ojoCMDefinedParts.

	^   #( window 'Ejemplo CMDefinedPanelAppModel'
	nil
	nil
	nil
	nil
	nil
'Ejemplo CMDefinedPanleAppModel'
	exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
	32 32 150 100 640 480 1400 1000
	CMDefinedPartsInstaller exampleWithMetaInfoDefinedWindowStore
	allAllowed
	nil
	( refToModel 'KronoSimple' kronoSimpleStore KRSimpleMetaInfoHolder )

	  (allowedRootTypes
		 ( refToType 'Clinica' 'Nucleo'  ) 
	   )
	  nil
	(parts
	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title1LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		cif exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		nil
		yourself left
		0
	   )

	  ( field 'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field1LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'cif'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title2LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		razonSocial exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		nil
		yourself left
		0
	   )

	  ( field 'Field 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field2LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'razonSocial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title3LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		nombreComercial exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		nil
		yourself left
		0
	   )

	  ( field 'Field 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field3LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'nombreComercial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	 )

   )!

ojoCMDefinedParts! !

!CMDefinedPanelAppModel class publicMethodsFor: 'examples'!

example01
	"CMDefinedPanelAppModel example01"

	|  aDefinedWindow anAppModel   anObject |

	anObject := self example01ObjectCMGO.
	anObject isNil ifTrue: [ ^self].

	aDefinedWindow := KRSimpleDefinedUIHolder currentDefinedUIStoreMethodSelectorBoundToModel: #exampleDefinedWindowStore.
aDefinedWindow browse.

	aDefinedWindow rebindToModel.
	anAppModel := self new.
	anAppModel definedWindow: aDefinedWindow.
	anAppModel connectOn: anObject.
	anAppModel open!

example01DefinedWindow
	"CMDefinedPanelAppModel example01DefinedWindow browse"

	| someParts aDefinedWindow aTitlePart aTitlePart2 aFieldPart1 |
	someParts := OrderedCollection new.
	
	aTitlePart := CMDefinedPartTitle
		named: 							'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.15 with: 0.25) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#title1NlsSymbol 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart.


	aTitlePart2 := CMDefinedPartTitle
		named: 							'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.45 with: 0.95) with: true)
		layoutFrame: 					nil
		nlsSymbol: 						#title2NlsSymbol 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart2.


	aFieldPart1 := CMDefinedPartField
		named: 							'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.15 with: 0.30) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:							nil
		valueExpression:				'sample content for Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart1.

	aDefinedWindow := CMDefinedWindow
			named: 'Ejemplo CMDefinedPanelAppModel'
			titleDefaultTranslation: 'Ejemplo CMDefinedPanleAppModel'
			titleNlsSymbol: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol 
			titleNlsGroup: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
			origin: 32@32 minExtent: 150@100 prefExtent: 640@480 maxExtent: 1400@1000
			storeClassName:					'CMDefinedPartsInstaller'
			storeMethodSelector:			'exampleWithMetaInfoDefinedWindowStore'
			parts: someParts.

	^aDefinedWindow!

example01DefinedWindowWithLiteralExpressionField
	"CMDefinedPanelAppModel example01DefinedWindow browse"

	| someParts aDefinedWindow aTitlePart aTitlePart2 aFieldPart1 |
	someParts := OrderedCollection new.
	
	aTitlePart := CMDefinedPartTitle
		named: 							'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.15 with: 0.25) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#title1NlsSymbol 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart.


	aTitlePart2 := CMDefinedPartTitle
		named: 							'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.45 with: 0.95) with: true)
		layoutFrame: 					nil
		nlsSymbol: 						#title2NlsSymbol 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart2.


	aFieldPart1 := CMDefinedPartField
		named: 							'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.15 with: 0.30) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:							nil
		valueExpression:				'sample content for Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart1.

	aDefinedWindow := CMDefinedWindow
			named: 'Ejemplo CMDefinedPanelAppModel'
			titleDefaultTranslation: 'Ejemplo CMDefinedPanleAppModel'
			titleNlsSymbol: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol 
			titleNlsGroup: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
			origin: 32@32 minExtent: 150@100 prefExtent: 640@480 maxExtent: 1400@1000
			storeClassName:					'CMDefinedPartsInstaller'
			storeMethodSelector:			'exampleWithMetaInfoDefinedWindowStore'
			parts: someParts.

	^aDefinedWindow!

example01DefinedWindowWithMetaInfoField: theObjectMetaInfo

	"CMDefinedPanelAppModel example01DefinedWindow browse"

	| someParts aDefinedWindow aTitlePart2 aFieldPart1 aFieldPart2 aFieldPart3 aTitlePart1 aTitlePart3 aSep1 aSep2 aSep3 |

	theObjectMetaInfo isNil ifTrue: [ ^nil].

	someParts := OrderedCollection new.
	
	aSep1 := CMDefinedPartSep named: 'sep1'.
	aSep1	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep1.

	aTitlePart1 := CMDefinedPartTitle
		named: 							'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#cif 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart1.


	aFieldPart1 := CMDefinedPartField
		named: 							'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:							(theObjectMetaInfo effectiveFeatureNamed: 'cif')
		valueExpression:				nil
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart1.

	aSep2 := CMDefinedPartSep named: 'sep2'.
	aSep2	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep2.

	aTitlePart2 := CMDefinedPartTitle
		named: 							'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#razonSocial 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart2.


	aFieldPart2 := CMDefinedPartField
		named: 							'Field 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:							(theObjectMetaInfo effectiveFeatureNamed: 'razonSocial')
		valueExpression:				nil
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart2.


	aSep3 := CMDefinedPartSep named: 'sep3'.
	aSep3	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep3.

	aTitlePart3 := CMDefinedPartTitle
		named: 							'Title 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title3LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title3LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#nombreComercial 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart3.

	aFieldPart3 := CMDefinedPartField
		named: 							'Field 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field3LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field3LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:							(theObjectMetaInfo effectiveFeatureNamed: 'nombreComercial')
		valueExpression:				nil
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart3.

	aDefinedWindow := CMDefinedWindow
			named: 'Ejemplo CMDefinedPanelAppModel'
			titleDefaultTranslation: 'Ejemplo CMDefinedPanleAppModel'
			titleNlsSymbol: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol 
			titleNlsGroup: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
			origin: 32@32 minExtent: 150@100 prefExtent: 640@480 maxExtent: 1400@1000
			storeClassName:			'CMDefinedPartsInstaller'
			storeMethodSelector:		'exampleWithMetaInfoDefinedWindowStore'
			parts: someParts.
	aDefinedWindow model:   theObjectMetaInfo model.
self halt.
	aDefinedWindow allowedRootTypesAdd: theObjectMetaInfo.
	aDefinedWindow allowMode: CMDefinedPart allowModeAllAllowedSymbol.


	^aDefinedWindow!

example01DefinedWindowWithMetaInfoRefs

	"CMDefinedPanelAppModel example01DefinedWindowWithMetaInfoRefs browse"

	| someParts aDefinedWindow aTitlePart2 aFieldPart1 aFieldPart2 aFieldPart3 aTitlePart1 aTitlePart3 aSep1 aSep2 aSep3 |

	someParts := OrderedCollection new.
	
	aSep1 := CMDefinedPartSep named: 'sep1'.
	aSep1	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep1.

	aTitlePart1 := CMDefinedPartTitle
		named: 							'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#cif 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart1.


	aFieldPart1 := CMDefinedPartField
		named: 							'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:	#( refToAttribute 'cif'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 
		valueExpression:				nil
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart1.

	aSep2 := CMDefinedPartSep named: 'sep2'.
	aSep2	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep2.

	aTitlePart2 := CMDefinedPartTitle
		named: 							'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#razonSocial 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart2.


	aFieldPart2 := CMDefinedPartField
		named: 							'Field 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:	#( refToAttribute 'razonSocial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 
		valueExpression:				nil
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart2.


	aSep3 := CMDefinedPartSep named: 'sep3'.
	aSep3	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep3.

	aTitlePart3 := CMDefinedPartTitle
		named: 							'Title 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title3LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title3LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#nombreComercial 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart3.

	aFieldPart3 := CMDefinedPartField
		named: 							'Field 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field3LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field3LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							nil
		change: 							nil
		metaInfo:	#( refToAttribute 'nombreComercial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 
		valueExpression:				nil
		menu: 							nil
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart3.

	aDefinedWindow := CMDefinedWindow
			named: 'Ejemplo CMDefinedPanelAppModel'
			titleDefaultTranslation: 'Ejemplo CMDefinedPanleAppModel'
			titleNlsSymbol: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol 
			titleNlsGroup: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
			origin: 32@32 minExtent: 150@100 prefExtent: 640@480 maxExtent: 1400@1000
			storeClassName:			'CMDefinedPartsInstaller'
			storeMethodSelector:		'exampleWithMetaInfoDefinedWindowStore'
			parts: someParts.
	aDefinedWindow  forzeModelRefTmpValues:   #( refToModel 'KronoSimple' kronoSimpleStore KRSimpleMetaInfoHolder ).
	aDefinedWindow forzeAllowedRootTypesRefsTmpValues: # (allowedRootTypes ( refToType 'Clinica' 'Nucleo'  ) ).
	aDefinedWindow allowMode: CMDefinedPart allowModeAllAllowedSymbol.


	^aDefinedWindow!

example01ObjectCMGO
	"CMDefinedPanelAppModel example01ObjectCMGO"

	^KRSimpleInfoHolder  currentInfoClinica! !

!CMDefinedPanelAppModel class publicMethodsFor: 'interface specs'!

dynamicWindowSpec
	| aFullSpec aWindowSpec aComponentSpec aLookPreferences |

	aFullSpec := FullSpec new.

	aLookPreferences := UserParameters colorLookPreferencesFor: #window.

	aWindowSpec := WindowSpec new.
	aWindowSpec 
		label: ' CMDefinedPanelAppModelExampleWindowSpec';
		min: self  windowMinExtent;
		bounds: self windowInitialBounds;
		colors: aLookPreferences.

	aComponentSpec := SpecCollection new.

	aFullSpec window: aWindowSpec.
	aFullSpec component: aComponentSpec.

	^aFullSpec!

windowInitialBounds
	^0@0 corner: 640@480!

windowMinBounds
	^0@0 corner: self windowMinExtent!

windowMinExtent
	^160@120!

windowSpec
	| aFullSpec aWindowSpec aComponentSpec aLookPreferences |

	aFullSpec := FullSpec new.

	aLookPreferences := UserParameters colorLookPreferencesFor: #window.

	aWindowSpec := WindowSpec new.
	aWindowSpec 
		label: ' CMDefinedPanelAppModelExampleWindowSpec';
		min: self  windowMinExtent;
		bounds: self windowInitialBounds;
		colors: aLookPreferences.

	aComponentSpec := SpecCollection new.

	aFullSpec window: aWindowSpec.
	aFullSpec component: aComponentSpec.

	^aFullSpec! !

!CMDefinedPanelAppModel publicMethodsFor: 'accessing'!

definedWindow
	^definedWindow!

definedWindow: theDefinedWindow
	definedWindow := theDefinedWindow! !

!CMDefinedPanelAppModel publicMethodsFor: 'adaptors'!

actionAdaptorForPart: thePart
	| aCache anAdaptor aPartMetaInfo anActionExpression |

	thePart isNil ifTrue: [ ^nil].

	aCache := self adaptorsCache.

	aPartMetaInfo := thePart metaInfo.
	^aPartMetaInfo isNil 
		ifFalse: [ 
			anAdaptor := aCache at: aPartMetaInfo ifAbsent: [ nil].
			anAdaptor isNil ifFalse: [ ^anAdaptor].

			anAdaptor := thePart buildActionAdaptorFrom: self.
			anAdaptor isNil ifTrue: [ ^nil].

			anAdaptor subjectChannel: objectHolder.

			aCache at: aPartMetaInfo put: anAdaptor.
			anAdaptor
		]
		ifTrue: [ 
			anActionExpression := thePart actionExpression.
			(anActionExpression isNil or: [ anActionExpression isEmpty]) ifTrue: [ ^nil].

			anAdaptor := aCache at: anActionExpression ifAbsent: [ nil].
			anAdaptor isNil ifFalse: [ ^anAdaptor].

			anAdaptor := thePart buildActionAdaptorFrom: self.
			anAdaptor isNil ifTrue: [ ^nil].

			anAdaptor subjectChannel: objectHolder.

			aCache at: anActionExpression put: anAdaptor.
			anAdaptor
		]!

adaptorsCache
	adaptorsCache isNil ifTrue: [ adaptorsCache := IdentityDictionary new].
	^adaptorsCache!

enablementAdaptorForPart: thePart
	| aCache anAdaptor aPartEnablementPredicate |

	thePart isNil ifTrue: [ ^nil].

	aPartEnablementPredicate := thePart enablementPredicate.
	(aPartEnablementPredicate isNil or: [ aPartEnablementPredicate isEmpty]) ifTrue: [ ^nil].

	aCache := self adaptorsCache.
	anAdaptor := aCache at: aPartEnablementPredicate ifAbsent: [ nil].
	anAdaptor isNil ifFalse: [ ^anAdaptor].

	anAdaptor := thePart buildEnablementAdaptorFrom: self.
	anAdaptor isNil ifTrue: [ ^nil].

	anAdaptor subjectChannel: objectHolder.

	aCache at: aPartEnablementPredicate put: anAdaptor.
	^anAdaptor!

valueAdaptorForPart: thePart
	| aCache anAdaptor aPartMetaInfo aValueExpression |

	thePart isNil ifTrue: [ ^nil].

	aCache := self adaptorsCache.

	aPartMetaInfo := thePart metaInfo.
	^aPartMetaInfo isNil 
		ifFalse: [ 
			anAdaptor := aCache at: aPartMetaInfo ifAbsent: [ nil].
			anAdaptor isNil ifFalse: [ ^anAdaptor].

			anAdaptor := thePart buildValueAdaptorFrom: self.
			anAdaptor isNil ifTrue: [ ^nil].

			anAdaptor subjectChannel: objectHolder.

			aCache at: aPartMetaInfo put: anAdaptor.
			anAdaptor
		]
		ifTrue: [ 
			aValueExpression := thePart valueExpression.
			(aValueExpression isNil or: [ aValueExpression isEmpty]) ifTrue: [ ^nil].

			anAdaptor := aCache at: aValueExpression ifAbsent: [ nil].
			anAdaptor isNil ifFalse: [ ^anAdaptor].

			anAdaptor := thePart buildValueAdaptorFrom: self.
			anAdaptor isNil ifTrue: [ ^nil].

			anAdaptor subjectChannel: objectHolder.

			aCache at: aValueExpression put: anAdaptor.
			anAdaptor
		]!

visibilityAdaptorForPart: thePart
	| aCache anAdaptor aPartVisibilityPredicate |

	thePart isNil ifTrue: [ ^nil].

	aPartVisibilityPredicate := thePart visibilityPredicate.
	aPartVisibilityPredicate == true ifTrue: [ ^nil].
	aPartVisibilityPredicate == false ifTrue: [ ^nil].
	(aPartVisibilityPredicate isNil or: [ aPartVisibilityPredicate isEmpty]) ifTrue: [ ^nil].

	aCache := self adaptorsCache.
	anAdaptor := aCache at: aPartVisibilityPredicate ifAbsent: [ nil].
	anAdaptor isNil ifFalse: [ ^anAdaptor].

	anAdaptor := thePart buildVisibilityAdaptorFrom: self.
	anAdaptor isNil ifTrue: [ ^nil].

	anAdaptor subjectChannel: objectHolder.

	aCache at: aPartVisibilityPredicate put: anAdaptor.
	^anAdaptor! !

!CMDefinedPanelAppModel publicMethodsFor: 'connecting'!

connectOn: theObject
	
	object := theObject.! !

!CMDefinedPanelAppModel publicMethodsFor: 'initialize-release'!

initialize
	super initialize.! !

!CMDefinedPanelAppModel publicMethodsFor: 'interface opening'!

partsForWindow: laDefinedWindow
	^laDefinedWindow parts!

postBuildWith: theBuilder 

	| aDefinedWindow aLayoutRectangles unasDefinedParts |

	super postBuildWith: theBuilder.

	aDefinedWindow := self definedWindow.
	aLayoutRectangles := aDefinedWindow layoutRectangles.

	unasDefinedParts := self partsForWindow: aDefinedWindow.

	unasDefinedParts do: [:unaDefinedPart |
		unaDefinedPart addDefinedTo: self with: theBuilder withDefaultLayouts: aLayoutRectangles]! !

!CMDefinedPanelAppModel publicMethodsFor: 'layout support'!

allLayoutsFor: elComposedLayout withDefaultLayouts: theLayoutRectangles
	| unaSumaVertExtents unNuevoAllLayouts unYCursor   unaMinWidth unaMinHeight otroComponent |

	^self layoutsCacheDictionary at: elComposedLayout ifAbsent: [ | unosLayouts |

	unosLayouts := (theLayoutRectangles isNil or: [ theLayoutRectangles isEmpty]) 
		ifTrue: [  self defaultLayoutsFor: elComposedLayout] 
		ifFalse: [ theLayoutRectangles].

	"name, relative height, min extent point , max extent point, #(relXStart relXWidth) , must advance row"		

	unaSumaVertExtents := 0.
	unaSumaVertExtents :=  unosLayouts inject: unaSumaVertExtents 
		into: [ :unaSuma :unComponent | unaSuma + 
			((unComponent at: 6)  
				ifTrue: [ unComponent at: 2] 
				ifFalse: [ 0])
		].
	unNuevoAllLayouts := OrderedCollection new: (unosLayouts size).

	unaMinWidth := 0.
	unaMinHeight := 0.
	unYCursor := 0.
	unosLayouts do: [:unComponent |  | unHeight |
		unaMinWidth := unaMinWidth max: (unComponent at: 3) x.
		(unComponent at: 6) ifTrue: [ 
			unaMinHeight := unaMinHeight + (unComponent at: 3) y
		].
		unHeight := (unComponent at: 2) /  unaSumaVertExtents.
		otroComponent := ((OrderedCollection new: 6) 
				 "name, relative rect, minimum extent point, maximum extent point , absolute rect justification (si es de columna)"
				addLast: (unComponent at: 1);
				addLast: ( ((unComponent at: 5)  at: 1)@ unYCursor 
						extent: ((unComponent at: 5) at: 2) @ unHeight);
				addLast: (unComponent at: 3);
				addLast: (unComponent at: 4);
				addLast: nil;
				addLast: (unComponent at: 6);
				yourself
			).
		unComponent size > 6 ifTrue: [ otroComponent addLast: (unComponent at: 7)].
		unNuevoAllLayouts addLast:  otroComponent.

		(unComponent at: 6)  ifTrue: [ unYCursor := unYCursor + unHeight].
	].
	"Dictionary : key: ComposedLayoutSymbol value :
		minimumSize, array of layouts , absoluteRectangle"
	self layoutsCacheDictionary at: elComposedLayout put: 
		(Array with: unaMinWidth@unaMinHeight with: unNuevoAllLayouts with: nil)
	]
"*VIPVersion 22-6-97 | 8:10:57 pm 'ACV'*"!

invalidateLayoutsCacheDictionary

	layoutsCacheDictionary isNil ifFalse: [ 
		layoutsCacheDictionary keys copy do: [:unaKey | layoutsCacheDictionary removeKey: unaKey]
	].
	layoutsCacheDictionary 			:= Dictionary new: 4.!

layoutFor: elComponent in: elComposedLayout withDefaultLayouts: theLayoutRectangles

	|  unLayout |

	unLayout := ((self allLayoutsFor: elComposedLayout withDefaultLayouts: theLayoutRectangles) at: 2) detect: [:otroLayout | 
		(otroLayout at: 1) = elComponent 
	] ifNone: [ self errorSignal raiseWith: 'Layout : ', elComposedLayout,  ' Component ', elComponent , ' not found'].
	
	^unLayout size > 6 
		ifTrue: [
			CooperativeRectangle 
				origin: 		(unLayout at: 2) origin 
				extent: 		(unLayout at: 2) extent 
				name: 			(unLayout at: 1) 
				layout: 		elComposedLayout 
				solver: 		self
				justification: 	(unLayout at: 7)	
				withLayoutRectangles: theLayoutRectangles
		]
		ifFalse: [
			CooperativeRectangle 
				origin: 		(unLayout at: 2) origin 
				extent: 		(unLayout at: 2) extent 
				name: 			(unLayout at: 1) 
				layout: 		elComposedLayout 
				solver: 		self
				withLayoutRectangles: theLayoutRectangles
		]!

layoutsCacheDictionary

	layoutsCacheDictionary isNil ifTrue:  [ layoutsCacheDictionary := Dictionary new: 4].
	^layoutsCacheDictionary!

layoutsIn: elComposedLayout withDefaultLayouts: theLayoutRectangles

	^((self allLayoutsFor: elComposedLayout withDefaultLayouts: theLayoutRectangles) at: 2) collect: [:unLayout | 

		unLayout size > 6 
			ifTrue: [
				CooperativeRectangle 
					origin: (unLayout at: 2) origin 
					extent: (unLayout at: 2) extent 
					name: (unLayout at: 1) 
					layout: elComposedLayout 
					solver: self
					justification:	(unLayout at: 7)
				withLayoutRectangles: theLayoutRectangles
			]
			ifFalse: [
				CooperativeRectangle 
					origin: (unLayout at: 2) origin 
					extent: (unLayout at: 2) extent 
					name: (unLayout at: 1) 
					layout: elComposedLayout 
					solver: self
				withLayoutRectangles: theLayoutRectangles
			]
	]
"*VIPVersion 22-6-97 | 8:10:58 pm 'ACV'*"!

rectangle: elCooperativeRectangle relativeTo: elAbsoluteRectangle withDefaultLayouts: theLayoutRectangles
	 
	| unExtraHeight  unYAbsoluteCursor unHeight unNewYAbsoluteCursor unOtraExtraHeight unSumRelativeHeightWithoutMaximum unosAllLayouts |

	unosAllLayouts := self allLayoutsFor: elCooperativeRectangle layoutName withDefaultLayouts: theLayoutRectangles.
	(unosAllLayouts at: 3) = elAbsoluteRectangle 
		ifTrue: [
			^((unosAllLayouts at: 2) detect: [:unLayout |
				(unLayout at: 1) = elCooperativeRectangle rectangleName]) at: 5
		].

	 "name, relative rect, minimum extent point, maximum extent point , absolute rect"
	
	(unExtraHeight := elAbsoluteRectangle height - (unosAllLayouts at: 1)  y ) < 0 
		ifTrue: [  "InputState default leftShiftDown ifFalse: [ ^nil]" ].
	unYAbsoluteCursor := 0. 
	unSumRelativeHeightWithoutMaximum := 0.
	(unosAllLayouts at: 2) do: [:unLayout |
	 		unHeight := ((unLayout at: 3) y + (unExtraHeight * (unLayout at: 2) height)) rounded.
			(unLayout at: 4) y  = 0 
				ifFalse: [ 
					unHeight := (unHeight  min: (unLayout at: 4) y) rounded
				]
				ifTrue: [
					(unLayout at: 6) ifTrue: [
						unSumRelativeHeightWithoutMaximum := 
							unSumRelativeHeightWithoutMaximum + (unLayout at: 2) height
					]
				].
		(unLayout at: 6) ifTrue: [
			unYAbsoluteCursor := unYAbsoluteCursor + unHeight
		]
	].

	unOtraExtraHeight := (elAbsoluteRectangle height - unYAbsoluteCursor) rounded.


	unOtraExtraHeight  < 0 ifTrue: [ unOtraExtraHeight := 0].
	unYAbsoluteCursor := 0.
	(unosAllLayouts at: 2) do: [:unLayout |
		unHeight := ((unLayout at: 3) y + (unExtraHeight * (unLayout at: 2) height)) rounded.
		(unLayout at: 4) y = 0
			ifFalse: [ 
				unHeight := (unHeight  min: (unLayout at: 4) y) rounded
			]
			ifTrue: [
				unHeight := (unHeight +  (unOtraExtraHeight * 
					((unLayout at: 2) height / unSumRelativeHeightWithoutMaximum))) rounded
			].
		unNewYAbsoluteCursor := unYAbsoluteCursor + unHeight.
		unLayout at: 5 put: (Rectangle 
			left:  (elAbsoluteRectangle left + 
				(elAbsoluteRectangle width * (unLayout at: 2) left)) rounded
			right: (elAbsoluteRectangle left + 
				(elAbsoluteRectangle width * (unLayout at: 2) right)) rounded
			top:  unYAbsoluteCursor 
			bottom: unNewYAbsoluteCursor
		).
		(unLayout at: 6) ifTrue: [
			unYAbsoluteCursor := unNewYAbsoluteCursor	
		].
	].

	unosAllLayouts at: 3 put: elAbsoluteRectangle.

	^self rectangle: elCooperativeRectangle relativeTo: elAbsoluteRectangle withDefaultLayouts: theLayoutRectangles!

refreshLayoutChanges

	self invalidateLayoutsCacheDictionary.

	(self builder composite)
		layoutComponentsForBounds: self builder composite bounds;
		invalidate.
	self builder window extentEvent: self builder window displayBox extent + (1@1).!

refreshWindow

	builder window controller display.! !

!CMDefinedPanelAppModel privateMethodsFor: 'layout support'!

unfilteredLayoutFor: elComponent in: elComposedLayout
	"Views"
	|  unLayout |
self halt.
	unLayout := ((self unfilteredAllLayoutsFor: elComposedLayout) at: 2) detect: [:otroLayout | 
		(otroLayout at: 1) = elComponent
	] ifNone: [ (self errorSignal raiseWith: 'Layout : ', elComposedLayout, 
		' Component ', elComponent , ' not found')
	].
	
	^unLayout size > 6 
		ifTrue: [
			CooperativeRectangle 
				origin: 		(unLayout at: 2) origin 
				extent: 		(unLayout at: 2) extent 
				name: 			(unLayout at: 1) 
				layout: 		elComposedLayout 
				solver: 		self
				justification: 	(unLayout at: 7)
		]
		ifFalse: [
			CooperativeRectangle 
				origin: 		(unLayout at: 2) origin 
				extent: 		(unLayout at: 2) extent 
				name: 			(unLayout at: 1) 
				layout: 		elComposedLayout 
				solver: 		self
		]

"*VIPVersion 22-6-97 | 8:10:59 pm 'ACV'*"!

unfilteredLayoutFor: elComponent in: elComposedLayout withDefaultLayouts: theLayoutRectangles
	"Views"
	|  unLayout |
	unLayout := ((self unfilteredAllLayoutsFor: elComposedLayout) at: 2) detect: [:otroLayout | 
		(otroLayout at: 1) = elComponent
	] ifNone: [ (self errorSignal raiseWith: 'Layout : ', elComposedLayout, 
		' Component ', elComponent , ' not found')
	].
	
	^unLayout size > 6 
		ifTrue: [
			CooperativeRectangle 
				origin: 		(unLayout at: 2) origin 
				extent: 		(unLayout at: 2) extent 
				name: 			(unLayout at: 1) 
				layout: 		elComposedLayout 
				solver: 		self
				justification: 	(unLayout at: 7)
				withLayoutRectangles: theLayoutRectangles

		]
		ifFalse: [
			CooperativeRectangle 
				origin: 		(unLayout at: 2) origin 
				extent: 		(unLayout at: 2) extent 
				name: 			(unLayout at: 1) 
				layout: 		elComposedLayout 
				solver: 		self
				withLayoutRectangles: theLayoutRectangles
		]

"*VIPVersion 22-6-97 | 8:10:59 pm 'ACV'*"!

unfilteredMinimumSizeFor: elComposedLayout

	^(self unfilteredAllLayoutsFor: elComposedLayout) at: 1

"*VIPVersion 22-6-97 | 8:10:59 pm 'ACV'*"! !

!CMDefinedPanelAppModel publicMethodsFor: 'nls suport'!

nls: elSymbol group: elGroup
	^TranslationsHome  nlsApp: self class class name asString group: elGroup item: elSymbol! !

!CMDefinedPanelAppModel publicMethodsFor: 'private'!

initializeAdaptors
	super initializeAdaptors.! !

!CMDefinedPart class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Elemento'!

kind
	^#part! !

!CMDefinedPart class publicMethodsFor: 'catalogs'!

classesForMETAEditor
	"CMDefinedPart classesForMetaEditor"
	"METAChildSpecAutoEditor openOn: CMDefinedPart classesForMETAEditor selector: nil."

	^#(CMDefinedPart CMDefinedWindow CMDefinedPartGroup CMDefinedPartTitle  CMDefinedPartField CMDefinedPartCode   CMDefinedPartLongField   CMDefinedPartButton CMDefinedPartSwitch  CMDefinedPartList CMDefinedPartMultiList CMDefinedPartSlider CMDefinedPartXYSlider   CMDefinedPartTranscript CMDefinedPartImage CMDefinedPartSep CMDefinedPartColumn)! !

!CMDefinedPart class publicMethodsFor: 'class initialization'!

initialize
	"CMDefinedPart initialize"
	"CMDefinedPart withAllSubclassesDo: [:aC | aC initialize]"

	| someClasses someLabels |
	someClasses := self allSubclasses.
	someLabels := someClasses collect: [:aClass | aClass kind].
 
	MenuCMDefinedPartKinds  := PopUpMenu
		labelList: (Array with: (Array with: 'Cancelar')  with: someLabels ) 
		values:	(Array with: nil) ,  someClasses! !

!CMDefinedPart class publicMethodsFor: 'constants'!

allowModeAllAllowedSymbol
	^#allAllowed!

allowModeAllowAllSubTypesSymbol
	^#allowAllSubTypes!

allowModeAllowVirtualTypeMembersSymbol
	^#allowVirtualTypeMembers!

allowModeAllSymbol
	^#all!

allowModeByConstraintSymbol
	^#byConstraint!

allowModeNoneSymbol
	^#none!

allowModeOnlySubTypesSymbol
	^#onlySubTypes!

layoutModeCooperativeSymbol
	^#cooperative!

layoutModeFixedSymbol
	^#fixed! !

!CMDefinedPart class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true! !

!CMDefinedPart class publicMethodsFor: 'instance creation'!

named: elNombre
	| unaCMDefinedPart |
	unaCMDefinedPart := self new initialize.
	unaCMDefinedPart nombre: elNombre.
	^unaCMDefinedPart!

new
	"Generated by ISF/AD. Do not modify"
	^self basicNew initialize! !

!CMDefinedPart class publicMethodsFor: 'layout'!

layoutSymbolFromLCA: theLCA
	theLCA isNil ifTrue:  [ ^nil].
	theLCA size < 1 ifTrue: [ ^nil].
	^theLCA first! !

!CMDefinedPart class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^false!

menuCMDefinedPartKinds
	"CMDefinedPart menuCMDefinedPartKinds startUp"

	| someClasses someLabels someKindsAndClasses |

	someKindsAndClasses := (self definedPartKindsFactoryCache associations select: [:aA | 
		aA value isCMDefinedPartFactory
	]) asSortedCollection: [:aA :aB | aA key < aB key].
	someLabels := someKindsAndClasses collect: [:anAssoc | anAssoc key].
	someClasses := someKindsAndClasses collect: [:anAssoc | anAssoc value].

	^PopUpMenu
		labelList: (Array with: (Array with: 'Cancelar')  with: someLabels ) 
		values:	(Array with: nil) ,  someClasses! !

!CMDefinedPart class publicMethodsFor: 'navigation'!

defaultBrowserParameters
 
	^Dictionary new
		at:  METABrowser showCanvasLabelParameterSymbol put: true;
		yourself!

defaultDefinitionsHolder
	^MINDCountColoniesDefinitionsHolder forFeatureExtraction!

metaPerspectives

	^self metaPerspectivesGeneral!

metaPerspectivesGeneral


	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Name' 'Kind' 'IsDirty' )))
			nlsApp: 'CMDF';
			nlsGroup: 'General_Perspectives';
			nlsItem: 'General';
			nlsTranslation: 'General';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Enablement'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('VisibilityPredicate' 'InitiallyVisiblePredicate' 'EnablementPredicate' 'InitiallyEnabledPredicate')))
			nlsApp: 'CMDF';
			nlsGroup: 'General_Perspectives';
			nlsItem: 'Enablement';
			nlsTranslation: 'Disponibilidad';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'PersistIfDirty'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('IsDirty' 'DoPersistIfDirty')))
			nlsApp: 'CMDF';
			nlsGroup: 'General_Perspectives';
			nlsItem: 'PersistIfDirty';
			nlsTranslation: 'GrabarSiPendiente';
			yourself);
		yourself!

metaSelectors

	^self metaSelectorsGeneral!

metaSelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMDefinedPart selector: #metaSelectorsGeneral target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 5)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Name';
			basicSelector: #name;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Name';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'Name';
			nlsTranslation: 'Nombre';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Kind';
			basicSelector: #kind;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Kind';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'Kind';
			nlsTranslation: 'ClaseDeParte';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'IsDirty';
			basicSelector: #isDirty;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'IsDirty';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'IsDirty';
			nlsTranslation: 'EstaPendienteGrabar';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DoPersistIfDirty';
			basicSelector: #doPersistIfDirty;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DoPersistIfDirty';
			displaySelector: nil;
			canShowInTree: false;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'DoPersistIfDirty';
			nlsTranslation: 'GrabaSiPendiente';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'VisibilityPredicate';
			basicSelector: #visibilityPredicate;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'VisibilityPredicate';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'VisibilityPredicate';
			nlsTranslation: 'PredicadoVisibilidad';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'InitiallyVisiblePredicate';
			basicSelector: #initiallyVisiblePredicate;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'InitiallyVisiblePredicate';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'InitiallyVisiblePredicate';
			nlsTranslation: 'PredicadoVisibilidadInicial';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'EnablementPredicate';
			basicSelector: #enablementPredicate;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'EnablementPredicate';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'EnablementPredicate';
			nlsTranslation: 'PredicadoDisponibilidad';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'InitiallyEnabledPredicate';
			basicSelector: #initiallyEnabledPredicate;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'InitiallyEnabledPredicate';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPart_Selectors';
			nlsItem: 'InitiallyEnabledPredicate';
			nlsTranslation: 'PredicadoDisponibilidadInicial';
			yourself);

		yourself!

pathSelectors
	^self metaSelectors! !

!CMDefinedPart class publicMethodsFor: 'persistence-code'!

allowedRootTypesPersistenceSymbol
	^#allowedRootTypes!

allowedSubTypesPersistenceSymbol
	^#allowedSubTypes!

definedPartKindsFactoryCache
	"CMDefinedPart  definedPartKindsFactoryCache"

	| someSubclasses |
	(CMDefinedPartKindsFactoryCache isNil or: [CMDefinedPartKindsFactoryCache keys isEmpty]) ifFalse: [
		^CMDefinedPartKindsFactoryCache].

	someSubclasses := self allSubclasses.
	
	CMDefinedPartKindsFactoryCache := IdentityDictionary new: someSubclasses size * 2 + 1.
	someSubclasses do: [:aSC | CMDefinedPartKindsFactoryCache at: aSC kind put: aSC].
	^CMDefinedPartKindsFactoryCache!

example
	
	| aSameLayoutGroupsTestResult |
	METAScopedApplicationBrowser openForObject:  (PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor).

	METAScopedApplicationBrowser openForObject: 
		(CMDefinedPart halt newFromPersistenceAsCode: 
			(Compiler evaluate: (PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor) persistenceAsCodeString)).


	aSameLayoutGroupsTestResult := (PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor) 
		reportSubPartsHaveSameLayoutGroups.
	aSameLayoutGroupsTestResult isNil ifFalse: [
		Dialog warn: aSameLayoutGroupsTestResult first kind , ' ',  aSameLayoutGroupsTestResult first name , 
			'\has more than one Group because\' withCRs,
			(aSameLayoutGroupsTestResult at: 2) kind , ' ',  (aSameLayoutGroupsTestResult at: 2) name
	].


[ | anEditor aCMDefinedWindow |
	anEditor := InterModelUpdater interModelUpdater models detect: [:aM | aM isKindOf: ClinicaHistoriaEditor] ifNone: [nil].
	aCMDefinedWindow := anEditor definedWindow.
	aCMDefinedWindow  initializeLayoutsFromEditor: anEditor
] value.!

factoryFromPersistenceAsCodeKind: theCodeKind

	^self definedPartKindsFactoryCache at: theCodeKind ifAbsent: [ nil]!

indentStringForPersistenceAsCode
	^'  ' copy!

layoutFramePersistenceSymbol
	^#layoutFrame!

layoutRectanglePersistenceSymbol
	^#layoutRectangle!

layoutsBlockPersistenceSymbol
	^#layouts!

newFromPersistenceAsCode: theValues
	| aKind aFactory |
	theValues isNil ifTrue: [ ^nil].
	theValues isEmpty ifTrue: [ ^nil].
	
	aKind := theValues first.
		
	aFactory := self factoryFromPersistenceAsCodeKind: aKind.
	(aFactory isNil or: [ aFactory == CMDefinedPart]) ifFalse: [ ^aFactory  newPartFromFactoryFromPersistenceAsCode: theValues].

	aKind = self partsPersistenceSymbol  ifTrue: [ ^self newPartsCollectionFromPersistenceAsCode: theValues].
	aKind = self subPartsPersistenceSymbol  ifTrue: [ ^self newSubPartsCollectionFromPersistenceAsCode: theValues].

	aKind = self refToGroupMethodKind ifTrue: [ ^self newFromPersistenceAsStoredMethod: theValues].

	^nil!

newFromPersistenceAsStoredMethod:  theValues
| aGroupName aMethodSelector aClassName aClass someValues aKind aPartName aCMDefinedPartGroup |

	(theValues isNil or: [ theValues size < 4]) ifTrue: [ ^nil].

	theValues first = self refToGroupMethodKind ifFalse: [ ^nil].
	aGroupName := (theValues at: 2) asString.
	aGroupName isNil ifTrue: [ ^nil].
	aMethodSelector := (theValues at: 3) asSymbol.
	aMethodSelector isNil ifTrue: [ ^nil].
	aClassName := (theValues at: 4) asSymbol.
	aClassName isNil ifTrue: [ ^nil].
	
	aClass := Smalltalk at: aClassName asSymbol ifAbsent: [ nil].
	aClass isNil ifTrue: [ ^nil].

	someValues := aClass perform: aMethodSelector.
	(someValues isNil or: [ someValues size < 2])  ifTrue: [ ^nil].

	aKind := someValues first.
	(aKind isNil or: [ aKind = CMDefinedPartGroup kind]) ifFalse: [ ^nil].
	aPartName := someValues at: 2.
	aGroupName = aPartName ifFalse: [ ^nil].

	aCMDefinedPartGroup := CMDefinedPart newFromPersistenceAsCode: someValues.
	aCMDefinedPartGroup isNil ifTrue: [ ^nil].
	aCMDefinedPartGroup isGroup ifFalse: [ ^nil].
	aCMDefinedPartGroup name = aGroupName ifFalse: [ ^nil].
	^aCMDefinedPartGroup!

newPartFromFactoryFromPersistenceAsCode: theValues
	
	| aKind aName aNewPart |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].
	
	aKind := theValues first.
	aKind = self kind ifFalse: [ ^nil].

	aName := theValues size < 2  ifTrue: [ nil] ifFalse: [ theValues at: 2].
	aNewPart := self named: aName.
	aNewPart initFromValues: theValues.
	aNewPart cleanDirtyMark.
	^aNewPart!

newPartsCollectionFromPersistenceAsCode: theValues
	| aCollection  |
	theValues isNil ifTrue: [ ^nil].
	theValues isEmpty ifTrue: [ ^nil].
	
	self partsPersistenceSymbol =  theValues first ifFalse: [ ^nil].
		
	aCollection := OrderedCollection new: theValues size.
	2 to: theValues size do: [:anIndex | | anElem aNewPart |
		anElem := theValues at: anIndex.
		anElem isNil ifFalse: [ 
			aNewPart := 	self newFromPersistenceAsCode: anElem.
			aNewPart isNil ifFalse: [ aCollection add: aNewPart]]].
	^aCollection!

newSubPartsCollectionFromPersistenceAsCode: theValues
	| aCollection  |
	theValues isNil ifTrue: [ ^nil].
	theValues isEmpty ifTrue: [ ^nil].
	
	self  subPartsPersistenceSymbol =  theValues first ifFalse: [ ^nil].
		
	aCollection := OrderedCollection new: theValues size.
	2 to: theValues size do: [:anIndex | | anElem aNewPart |
		anElem := theValues at: anIndex.
		anElem isNil ifFalse: [ 
			aNewPart := 	self newFromPersistenceAsCode: anElem.
			aNewPart isNil ifFalse: [ aCollection add: aNewPart]]].
	^aCollection!

partsPersistenceSymbol
	^#parts!

preferredInstallerClass
	^CMDefinedPartsInstaller!

refToGroupMethodKind
	^#refToGroupMethod!

resetCMDefinedPartKindsFactoryCache
	"CMDefinedPart resetCMDefinedPartKindsFactoryCache"
	
	CMDefinedPartKindsFactoryCache := nil!

separatorForPersistenceAsCode
	^' ' copy!

subPartsPersistenceSymbol
	^#subParts! !

!CMDefinedPart class publicMethodsFor: 'preferences'!

preferredCMMetaInfoAspectAdaptorClass
	^CMDFMetaInfoAspectAdaptor!

preferredCMValueExpressionAspectAdaptorClass
	^CMDFValueExpressionAspectAdaptor! !

!CMDefinedPart publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^self class claseCMDefinedPart!

colorSolver

	^UserParameters colorSolverFor: self kind!

comment
	^comment!

comment: theValue
	comment := theValue.
	self markDirty.
	self changed: #comment!

container
	"Generated by ISF/AD. Do not modify"
	^container!

container: aValue
	"Generated by ISF/AD. Do not modify"
	container == aValue ifTrue: [ ^self].
	self containerRelease.
	container := aValue.
	aValue == nil ifFalse: [aValue subPartsPrivateAdd: self].
	self markDirty.
	self changed: #container!

enablementPredicate
	^enablementPredicate!

enablementPredicate: elValor

	| unValor |
	unValor := 
		elValor == true ifTrue: [ true] ifFalse: [ 
		elValor == false ifTrue: [ false] ifFalse: [ 
		(elValor isNil or: [ elValor isEmpty]) ifTrue: [ nil]  ifFalse: [ 	
		elValor = true printString  ifTrue: [ true] ifFalse: [ 
		elValor = false printString 	ifTrue: [ false] ifFalse: [ elValor]]]]].
	
	unValor = enablementPredicate ifTrue: [ ^self].

	enablementPredicate := unValor.
	self markDirty.
	self changed: #enablementPredicate!

initiallyEnabledPredicate
	^initiallyEnabledPredicate!

initiallyEnabledPredicate: elValor

	| unValor |
	unValor := 
		elValor == true ifTrue: [ true] ifFalse: [ 
		elValor == false ifTrue: [ false] ifFalse: [ 
		(elValor isNil or: [ elValor isEmpty]) ifTrue: [ nil]  ifFalse: [ 	
		elValor = true printString  ifTrue: [ true] ifFalse: [ 
		elValor = false printString 	ifTrue: [ false] ifFalse: [ elValor]]]]].
	
	unValor = initiallyEnabledPredicate ifTrue: [ ^self].

	initiallyEnabledPredicate := unValor.
	self markDirty.
	self changed: #initiallyEnabledPredicate!

initiallyVisiblePredicate
	^initiallyVisiblePredicate!

initiallyVisiblePredicate: elValor

	| unValor |
	unValor := 
		elValor == true ifTrue: [ true] ifFalse: [ 
		elValor == false ifTrue: [ false] ifFalse: [ 
		(elValor isNil or: [ elValor isEmpty]) ifTrue: [ nil]  ifFalse: [ 	
		elValor = true printString  ifTrue: [ true] ifFalse: [ 
		elValor = false printString 	ifTrue: [ false] ifFalse: [ elValor]]]]].
	
	unValor = initiallyVisiblePredicate ifTrue: [ ^self].

	initiallyVisiblePredicate := unValor.
	self markDirty.
	self changed: #initiallyVisiblePredicate!

kind
	^self class kind
"*VIPVersion 22-6-97 | 7:34:36 pm 'ACV'*"!

name
	
	^self nombre
"*VIPVersion 22-6-97 | 7:34:36 pm 'ACV'*"!

name: elNombre
	
	self nombre: elNombre.
	self markDirty.
	self changed: #name!

nombre
	|  |
	^name 
"*VIPVersion 22-6-97 | 7:34:36 pm 'ACV'*"!

nombre: elNombre

	name := elNombre.
	self markDirty.
	self changed: #nombre!

sampleRemoteValue
		self subclassResponsibility!

timestamp
	^timestamp!

timestamp: theValue
	timestamp := theValue.
	self changed: #timestamp!

visibilityPredicate
	^visibilityPredicate!

visibilityPredicate: elValor

	| unValor |
	unValor := 
		elValor == true ifTrue: [ true] ifFalse: [ 
		elValor == false ifTrue: [ false] ifFalse: [ 
		(elValor isNil or: [ elValor isEmpty]) ifTrue: [ nil]  ifFalse: [ 	
		elValor = true printString  ifTrue: [ true] ifFalse: [ 
		elValor = false printString 	ifTrue: [ false] ifFalse: [ elValor]]]]].
	
	unValor = visibilityPredicate ifTrue: [ ^self].

	visibilityPredicate := unValor.
	self markDirty.
	self changed: #visibilityPredicate! !

!CMDefinedPart publicMethodsFor: 'adaptors'!

buildEnablementAdaptorFrom: theDefinedEditor

	| anAdaptor |

	anAdaptor := self preferredCMPredicateAdaptor newForPart: self  from: theDefinedEditor.
	^anAdaptor!

buildEnablementUpdater: theBuilder spec: theSpec

	| aEnablementAdaptor aEnablementUpdater |

	aEnablementAdaptor := theSpec visibilityAdaptor.
	aEnablementAdaptor isNil ifTrue: [^nil].

	aEnablementUpdater := BlockValue 
		block: [ | aVal aComponent |
			aVal := aEnablementAdaptor value.
			aComponent := theBuilder componentAt: theSpec name.
			aComponent isNil ifFalse: [
				aComponent isEnabled: aVal == true
			].
		aVal
		]
		arguments: (Array with: aEnablementAdaptor).

	aEnablementAdaptor rememberToReleaseUpdater: aEnablementUpdater!

buildVisibilityAdaptorFrom: theDefinedEditor

	| anAdaptor |

	anAdaptor := self preferredCMPredicateAdaptor newForPart: self from: theDefinedEditor.
	^anAdaptor!

buildVisibilityUpdater: theBuilder spec: theSpec

	| aVisibilityAdaptor aVisibilityUpdater |

	aVisibilityAdaptor := theSpec visibilityAdaptor.
	aVisibilityAdaptor isNil ifTrue: [^nil].

	aVisibilityUpdater := BlockValue 
		block: [ | aVal aComponent |
			aVal := aVisibilityAdaptor value.
			aComponent := theBuilder componentAt: theSpec name.
			aComponent isNil ifFalse: [
				aComponent isVisible: aVal == true
			].
		aVal
		]
		arguments: (Array with: aVisibilityAdaptor).

	aVisibilityAdaptor rememberToReleaseUpdater: aVisibilityUpdater!

enablementAdaptorFrom: theDefinedEditor

	| anAdaptor |

	theDefinedEditor isNil ifTrue: [ ^nil].

	anAdaptor := theDefinedEditor enablementAdaptorForPart: self.
	^anAdaptor!

visibilityAdaptorFrom: theDefinedEditor

	| anAdaptor |

	theDefinedEditor isNil ifTrue: [ ^nil].

	anAdaptor := theDefinedEditor visibilityAdaptorForPart: self.
	^anAdaptor! !

!CMDefinedPart publicMethodsFor: 'association initialize-release'!

containerRelease
	"Generated by ISF/AD. Do not modify"
	container == nil
		ifFalse:
			[container subPartsPrivateRemove: self.
				container := nil.
				self changed: #container]! !

!CMDefinedPart publicMethodsFor: 'associations private'!

containerPrivate: aValue


	container := aValue.
	container isNil 
		ifTrue: [ self release]
		ifFalse: [ 
			self markDirty.
			self changed: #container]! !

!CMDefinedPart publicMethodsFor: 'ayuda'!

recursiveParts
	^#()
"*VIPVersion 22-6-97 | 7:34:38 pm 'ACV'*"!

recursivePartsFiltrando: elBloque
	^#()
"*VIPVersion 22-6-97 | 7:34:38 pm 'ACV'*"! !

!CMDefinedPart publicMethodsFor: 'derived accessing'!

allowedTypesForSubPart
	^nil!

containerAllowedTypes
	| aContainer someAllowedTypes |
	aContainer := self container.
	aContainer isNil ifTrue: [ ^nil].

	someAllowedTypes := aContainer allowedTypesForSubPart.
	^someAllowedTypes!

definedWindow
	| aTopPart |
	aTopPart := self topPart.
	aTopPart isWindow ifFalse: [ ^nil].
	^aTopPart!

model
	| aDefinedWindow aModel |

	aDefinedWindow := self definedWindow.
	aDefinedWindow isNil ifTrue: [ ^nil].

	aModel := aDefinedWindow model.
	^aModel!

topPart
	| aPart |
	aPart := self container.
	aPart isNil ifTrue: [ ^self].
	^aPart topPart! !

!CMDefinedPart publicMethodsFor: 'dirty'!

cleanDirtyMark
	dirty := false.	
	self changed: #dirty.!

doPersistIfDirty
	^false!

doPersistIfDirty: theValue

	theValue == true ifFalse: [ ^self].

	self persistIfDirty!

isDirty
	^dirty == true!

markDirty
	dirty == true ifFalse: [ 
		dirty := true.
		self changed: #dirty.
		timestamp := Time now.
		self changed: #timestamp.

		self container isNil ifFalse: [ self container markDirtyContainer]
	].!

markDirtyContainer
	self container isNil ifFalse: [ self container markDirtyContainer]!

persistIfDirty
	^container isNil ifFalse: [ container persistIfDirty] ifTrue: [ nil]! !

!CMDefinedPart publicMethodsFor: 'initialize-release'!

initialize
	super initialize.

	name	:= 'New ', ' CM Defined Part' , self class kind.

	self initTimestamp.

	dirty := false.
	container := nil.
	comment := nil.!

initTimestamp
	| aNow |
	aNow := Time now.
	timestamp := (Date today printFormat: #( 3 2 1 $/ 1 1)), ' ' , 
		aNow hours printString, ':' , 
		aNow minutes printString, ':' , 
		aNow seconds printString!

release
	
	name	:= nil.
	container	:= nil.
	dirty := nil.
	comment := nil.	

	super release! !

!CMDefinedPart publicMethodsFor: 'interface opening'!

addDefinedTo: theDefinedEditor with: theBuilder  withDefaultLayouts: aLayoutRectangles

	| aSpec |
	aSpec := self builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: aLayoutRectangles.
	aSpec isNil ifFalse: [ theBuilder add: aSpec]!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	self subclassResponsibility!

evaluateConstantPredicate: thePredicate

	| unValor unosStrings aString |
	(thePredicate isNil or: [ (thePredicate isKindOf: String) and: [ thePredicate isEmpty]]) ifTrue: [ ^false].

	unValor := (thePredicate = true or: [ thePredicate = false])
		ifTrue: [ thePredicate]
		ifFalse: [ 
			(thePredicate isNil or: [ thePredicate isEmpty])
				ifTrue: [ nil] 
				ifFalse: [ 	
					unosStrings := thePredicate  asArrayOfSubstrings.
					unosStrings isEmpty
						ifTrue: [  false]
						ifFalse: [ 
							aString := unosStrings size > 1  ifTrue: [ unosStrings first] ifFalse: [ false].
							aString = true printString ifTrue: [ true] ifFalse: [ 
							aString = false printString 	ifTrue: [ false] ifFalse: [ 
							false
						]]]
				]
		].

	^unValor! !

!CMDefinedPart publicMethodsFor: 'navigation'!

browse
	^METAScopedApplicationBrowser
		openForObject: 			self 
		"definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:		self defaultBrowserParameters
		beDialog:				false
		selectionOn:			nil asValue"!

browsePath

	^METAPathFinderScopedApplicationBrowser
		openForObject: 			self 
		"definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:		self defaultBrowserParameters
		beDialog:				false
		selectionOn:			nil asValue"!

defaultBrowserParameters
	^self class defaultBrowserParameters!

defaultDefinitionsHolder
	^self class defaultDefinitionsHolder!

metaClass

	^self class!

metaEditorClassLabel

	^self classLabelForMETAEditor!

metaNameSelector
	^#name!

metaSelectorsSelector
	^#metaSelectors

"*VIPVersion 15-7-97 | 8:30:48 pm 'ACV'*"! !

!CMDefinedPart publicMethodsFor: 'nls'!

isFromEditorClass
	^false!

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
	| aNLSSolver aTopPart |
	aTopPart := self topPart.
	aTopPart isNil ifTrue: [ ^nil].

	aNLSSolver := aTopPart nlsSolver.
	^aNLSSolver!

resolverItemTranslations

	| aNLSSolver someItemTranslations aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	someItemTranslations := OrderedCollection new: 2.

	aNLSItem := aNLSSolver nlsResolverItemApp: self nlsAppName group: self nameNLSGroupName item: self nameNLSItemName.
	aNLSItem isNil ifFalse: [ someItemTranslations add:  aNLSItem].

	^someItemTranslations! !

!CMDefinedPart publicMethodsFor: 'persistence-code'!

forgetMetaInfo!

indentStringForPersistenceAsCode
	^self class indentStringForPersistenceAsCode!

initFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	self comment:						(theValues size < 3 ifTrue: [nil] ifFalse: [ theValues at: 3]).  
	self visibilityPredicate:				(theValues size < 4 ifTrue: [nil] ifFalse: [ theValues at: 4]).  
	self initiallyVisiblePredicate:		(theValues size < 5 ifTrue: [nil] ifFalse: [ theValues at: 5]).  
	self enablementPredicate:			(theValues size < 6 ifTrue: [nil] ifFalse: [ theValues at: 6]).  
	self initiallyEnabledPredicate:		(theValues size < 7 ifTrue: [nil] ifFalse: [ theValues at: 7]).!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.
		
	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self comment );  cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self visibilityPredicate ); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self initiallyVisiblePredicate );  cr;  
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self enablementPredicate ); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self initiallyEnabledPredicate );  cr.!

numberPersistenceEntriesDefinedPart
	^7!

pcForV: theValue

	| aValue |
	(theValue isKindOf: Symbol) ifFalse: [
		((theValue isKindOf: String) or: [ theValue isKindOf: Integer]) ifTrue: [ ^theValue printString].
		aValue := theValue.
		(aValue isKindOf: Number) ifTrue: [ aValue := ((theValue * 1000)  asInteger / 1000) asFloat].
		^aValue printString
	].

	(theValue isKindOf: Symbol) ifTrue: [ 
		(theValue isEmpty or: [ theValue asString first isLetter not]) ifTrue: [ 
			^theValue printString
		]
	].

	^theValue asArrayOfSubstrings size > 1 
		ifTrue: [ theValue printString] 
		ifFalse: [ theValue asString]!

persistenceAsCodeStringOn: theStream indent: theIS

	| aSep |
	theStream isNil ifTrue: [ ^self]. 

	aSep := self separatorForPersistenceAsCode.
		
	theStream nextPutAll: theIS; nextPutAll: '( ';  nextPutAll: (self pcForV: self kind);  
		nextPutAll: aSep; nextPutAll: self name printString; cr.

	self localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream	nextPutAll: theIS; nextPutAll: ' )' ; cr; cr.!

preferredInstallerClass
	^self class preferredInstallerClass!

rebindToModelFromSolver: theSolver!

separatorForPersistenceAsCode
	^self class separatorForPersistenceAsCode!

unbindFromModel! !

!CMDefinedPart publicMethodsFor: 'printing'!

printOn: elStream
	"elStream nextPutAll: '(' , self class name , ' ', self nombre , ') '"

	^elStream nextPutAll: self nombre! !

!CMDefinedPart publicMethodsFor: 'save defined parts'!

symbolInEditor: elEditor
	^nil

"*VIPVersion 22-6-97 | 7:34:39 pm 'ACV'*"! !

!CMDefinedPart publicMethodsFor: 'tests'!

hasCooperativeLayout
	^false!

hasLayoutFrame
	^false!

hasLayoutRectangle
	^false!

isGroup
	^false!

isMultiList
	^false!

isNLS
	^false!

isSep
	^false!

isWindow
	^false! !

!CMDefinedPartAnonymous class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Sep'!

kind
	^#sep! !

!CMDefinedPartAnonymous class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartAnonymous class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesAnonymous!

metaPerspectivesAnonymous
	^OrderedCollection new

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsAnonymous!

metaSelectorsAnonymous
	
	"METAChildSpecAutoViewEditor openOn: CMDefinedPartSep selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.
	^(OrderedCollection new: 9)
		yourself! !

!CMDefinedPartAnonymous publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles
	^nil! !

!CMDefinedPartButton class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Boton'
"*VIPVersion 22-6-97 | 7:34:44 pm 'ACV'*"!

kind
	^#button
"*VIPVersion 22-6-97 | 7:34:44 pm 'ACV'*"! !

!CMDefinedPartButton class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 22-6-97 | 7:34:44 pm 'ACV'*"! !

!CMDefinedPartButton class publicMethodsFor: 'instance creation'!

named:								elNombre
	actionSelector:					elActionSelector
	nlsSymbol: 						elNlsSymbol 
	nlsGroup: 						elNlsGroup
	layoutSymbol:					elLayoutSymbol
	layoutGroup:					elLayoutGroup
	selectorVisible:					elSelectorVisible
	valorVisible:						elValorVisible
	selectorVisual:					elSelectorVisual
	valorVisual:						elValorVisual
	selectorActivo:					elSelectorActivo
	valorActivo:						elValorActivo


	^(super named: elNombre)
		actionSelector:					elActionSelector
		nlsSymbol: 						elNlsSymbol 
		nlsGroup: 							elNlsGroup
		layoutSymbol:						elLayoutSymbol
		layoutGroup:						elLayoutGroup;
		selectorVisible:					elSelectorVisible;
		valorVisible:						elValorVisible;
		selectorVisual:					elSelectorVisual;
		valorVisual:						elValorVisual;
		selectorActivo:					elSelectorActivo;
		valorActivo:						elValorActivo;
		yourself

"*VIPVersion 22-6-97 | 7:34:43 pm 'ACV'*"! !

!CMDefinedPartButton class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartButton class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesButton!

metaPerspectivesButton
	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('ActionSelector' 'ActionExpression')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartButton_Perspectives';
			nlsItem: 'Specific';
			nlsTranslation: 'Especificos de Boton';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'MetaInfo'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('MetaInfo' )))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartButton_Perspectives';
			nlsItem: 'MetaInfo';
			nlsTranslation: 'MetaInformacion';
			yourself);
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsButton!

metaSelectorsButton

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartButton selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 16)
		add: (METATerminalChildSpec new
			name: 'ActionSelector';
			basicSelector: #actionSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ActionSelector';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartButton_Selectors';
			nlsItem: 'ActionSelector';
			nlsTranslation: 'SelectorDeAccion';
			yourself);
		add: (METATerminalChildSpec new
			name: 'ActionExpression';
			basicSelector: #actionExpression;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ActionExpression';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartButton_Selectors';
			nlsItem: 'ActionExpression';
			nlsTranslation: 'ExpresionDeAccion';
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MetaInfo';
			basicSelector: #metaInfo;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Select;
			helpString: 'MetaInfo';
			displaySelector: nil;
			objectClassName: #CODEElement;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			nlsApp: 'CODE';
			nlsGroup: 'DefinedPartButton_Selectors';
			nlsItem: 'MetaInfo';
			nlsTranslation: 'MetaInformacion';
			yourself);
		yourself! !

!CMDefinedPartButton class publicMethodsFor: 'persistence-code'!

sizeSymbolAtValue: theValue
	theValue = 30 ifTrue: [ ^#minButtonHeight].
	theValue = 40 ifTrue: [ ^#midButtonHeight].
	theValue = 50 ifTrue: [ ^#bigButtonHeight].
	^nil!

sizeValueAtSymbol: theSymbol
	theSymbol = #minButtonHeight ifTrue: [ ^30].
	theSymbol = #midButtonHeight ifTrue: [ ^40].
	theSymbol = #bigButtonHeight ifTrue: [ ^50].
	^nil! !

!CMDefinedPartButton publicMethodsFor: 'accessing'!

actionExpression
	^actionExpression!

actionExpression: elValor

	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ elValor]
		].
	
	unValor = actionExpression  ifTrue: [ ^self].

	actionExpression := unValor.
	self markDirty.
	self changed: #actionExpression!

actionSelector
	^actionSelector
"*VIPVersion 22-6-97 | 7:34:42 pm 'ACV'*"!

actionSelector: elValor

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
	
	unValor = actionSelector  ifTrue: [ ^self].

	actionSelector := unValor.
	self markDirty.
	self changed: #actionSelector!

forzeActionExpression: elValor
	actionExpression := elValor!

forzeActionSelector: elValor

	actionSelector := elValor!

forzeMetaInfo: theMetaInfo

	metaInfo := theMetaInfo.!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo

	(theMetaInfo isKindOf: Array) ifTrue: [ 
		metaInfoRefTmpValues := theMetaInfo.
		^self
	].
	metaInfo := theMetaInfo.

	self markDirty.
	self changed: #metaInfo! !

!CMDefinedPartButton publicMethodsFor: 'adaptors'!

actionAdaptorFrom: theDefinedEditor

	| anAdaptor |

	theDefinedEditor isNil ifTrue: [ ^nil].

	anAdaptor := theDefinedEditor actionAdaptorForPart: self.
	^anAdaptor!

buildActionAdaptorFrom: theDefinedEditor

	| anAdaptor |

	anAdaptor := self preferredCMActionAdaptor newForPart: self from: theDefinedEditor.
	^anAdaptor! !

!CMDefinedPartButton publicMethodsFor: 'initialize-release'!

actionSelector:			elActionSelector
	nlsSymbol: 			elNlsSymbol 
	nlsGroup: 			elNlsGroup
	layoutSymbol:		elLayoutSymbol
	layoutGroup:		elLayoutGroup

	actionSelector 	:= elActionSelector.
	nlsSymbol			:= elNlsSymbol.
	nlsGroup			:= elNlsGroup.
	layoutSymbol		:= elLayoutSymbol.
	layoutGroup		:= elLayoutGroup.
	^self!

initialize
	super initialize.


	actionSelector	:= nil.!

release
	actionSelector	:= nil.
	super release! !

!CMDefinedPartButton publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder  withDefaultLayouts: theLayoutRectangles

	| aLayout aLabel anActionButtonSpec aLookPreferences aVisibilityAdaptor aEnablementAdaptor anActionAdaptor |

	aLayout := self hasLayoutFrame
		ifFalse: [ 
			self hasCooperativeLayout
				ifTrue: [ theDefinedEditor layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles]
				ifFalse: [ nil]
		]
		ifTrue: [ self layoutFrame ].

	aLayout isNil ifTrue: [ ^nil].

	aVisibilityAdaptor 		:= self visibilityAdaptorFrom: theDefinedEditor.
	aEnablementAdaptor 	:= self enablementAdaptorFrom: theDefinedEditor.

	anActionAdaptor := self actionAdaptorFrom: theDefinedEditor.
	anActionAdaptor isNil ifTrue: [ ^nil].

	aLabel := (UpdatableString
		nlsSolver: 	theDefinedEditor 
		symbol:   	self nlsSymbol 
		group:    	self nlsGroup) actualString.

	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.

	anActionButtonSpec := CMActionButtonSpec new.
	anActionButtonSpec
		name:  self name;
		layout: aLayout; 
		model: anActionAdaptor;
		hasImageOrientedLabel: false;
		setLabel: aLabel;
		colors: aLookPreferences;
		definedPart: self;
		definedEditor: theDefinedEditor;
		visibilityAdaptor: 				aVisibilityAdaptor;
		initiallyVisible:  			  		(self evaluateConstantPredicate: self initiallyVisiblePredicate);
		enablementAdaptor: 			aEnablementAdaptor
		initiallyEnabled:  				(self evaluateConstantPredicate: self initiallyEnabledPredicate).

	^anActionButtonSpec! !

!CMDefinedPartButton publicMethodsFor: 'persistence-code'!

firstPersistenceIndexButton
	^self numberPersistenceEntriesDefinedPart + 
		self numberPersistenceEntriesWithLayoutRectangle + 
		self numberPersistenceEntriesWithNLS + 
		1!

forgetMetaInfo

	self metaInfo: nil.
	self changed: #metaInfo!

initFromValues: theValues
	
	| aFPI |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	aFPI := self firstPersistenceIndexButton.

	self actionSelector: 			(theValues size < (aFPI + 0) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 0)]).
	self actionExpression: 			(theValues size < (aFPI + 1) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 1)]).

	theValues size < (aFPI + 2) ifFalse: [ self initReferencedMetaInfoFromValues: (theValues at: (aFPI + 2))]!

initMetaInfoRefTmpValuesFromModel 
	self metaInfo isNil ifTrue: [ ^self].

	metaInfoRefTmpValues := self metaInfo asReferenceArray!

initReferencedMetaInfoFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].
 
	theValues first = self class refToMetaInfoKindSymbol ifFalse: [ ^nil].

	metaInfoRefTmpValues := theValues.!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep aMetaInfo |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream nextPutAll: anIS;  nextPutAll: (self pcForV:  self  actionSelector ); cr.

	aMetaInfo := self metaInfo.
	aMetaInfo isNil  
		ifTrue: [ theStream nextPutAll: anIS; nextPutAll: (self pcForV: nil)] 
		ifFalse: [ aMetaInfo asReferenceAsCodeStringOn:  theStream indent: anIS]. 
	
	theStream cr.!

rebindReferencedMetaInfoValuesFromSolver: theSolver
	
	| someElements |

	(metaInfoRefTmpValues isNil or: [ metaInfoRefTmpValues isEmpty]) ifTrue: [ ^self].

	someElements := CODEElement resolveReferencedElementFromPersistenceAsCode: metaInfoRefTmpValues  
		solver: theSolver.
	self metaInfo: someElements.
	metaInfoRefTmpValues := nil!

rebindToModelFromSolver: theSolver

	self rebindReferencedMetaInfoValuesFromSolver: theSolver.!

unbindFromModel

	self initMetaInfoRefTmpValuesFromModel.! !

!CMDefinedPartCode class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Texto'
"*VIPVersion 22-6-97 | 7:34:51 pm 'ACV'*"!

kind
	^#code
"*VIPVersion 22-6-97 | 7:34:51 pm 'ACV'*"! !

!CMDefinedPartCode class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 22-6-97 | 7:34:50 pm 'ACV'*"! !

!CMDefinedPartCode class publicMethodsFor: 'instance creation'!

aspect: elAspect change: elChange menu: elMenu initialSelection: elInitialSelection layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup 

	^super new 
		aspect: elAspect 	change: elChange 	menu: elMenu 	initialSelection: elInitialSelection 	layoutSymbol: elLayoutSymbol 	layoutGroup: elLayoutGroup
"*VIPVersion 22-6-97 | 7:34:51 pm 'ACV'*"!

named: elNombre aspect: elAspect change: elChange menu: elMenu initialSelection: elInitialSelection layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup 

	^(super named: elNombre) 
		aspect: elAspect 	change: elChange 	menu: elMenu 	initialSelection: elInitialSelection 	layoutSymbol: elLayoutSymbol 	layoutGroup: elLayoutGroup
"*VIPVersion 22-6-97 | 7:34:51 pm 'ACV'*"!

named: 					elNombre
	aspect: 				elAspect
	change: 				elChange
	menu: 				elMenu
	initialSelection: 	elInitialSelection
	layoutSymbol: 		elLayoutSymbol
	layoutGroup: 		elLayoutGroup 
	selectorVisible:		elSelectorVisible
	valorVisible:			elValorVisible
	selectorVisual:		elSelectorVisual
	valorVisual:			elValorVisual
	selectorActivo:		elSelectorActivo
	valorActivo:			elValorActivo

	^(super named: elNombre) 
		aspect: 			elAspect
		change: 			elChange
		menu: 				elMenu
		initialSelection: 	elInitialSelection
		layoutSymbol: 	elLayoutSymbol
		layoutGroup: 		elLayoutGroup;
		selectorVisible:	elSelectorVisible;
		valorVisible:		elValorVisible;
		selectorVisual:	elSelectorVisual;
		valorVisual:		elValorVisual;
		selectorActivo:	elSelectorActivo;
		valorActivo:		elValorActivo;
		yourself
	
"*VIPVersion 22-6-97 | 7:34:51 pm 'ACV'*"! !

!CMDefinedPartCode class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartCode class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesCode!

metaPerspectivesCode
	^OrderedCollection new
		addLast: ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Aspect' 'Change' 'Menu' 'InitialSelection' 'ReadOnly')));

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsCode!

metaSelectorsCode

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartCode selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 14)
		add: (METATerminalChildSpec new
			name: 'Aspect';
			basicSelector: #aspect;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Aspect';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'Change';
			basicSelector: #change;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Change';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'Menu';
			basicSelector: #menu;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Menu';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'InitialSelection';
			basicSelector: #initialSelection;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'InitialSelection';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'ReadOnly';
			basicSelector: #readOnly;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ReadOnly';
			displaySelector: nil;
			yourself);
		yourself! !

!CMDefinedPartCode publicMethodsFor: 'accessing'!

aspect
	^aspect
"*VIPVersion 22-6-97 | 7:34:48 pm 'ACV'*"!

aspect: elValor

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
	
	unValor = aspect  ifTrue: [ ^self].

	aspect := unValor.
	self markDirty.
	self changed: #aspect!

change
	^change
"*VIPVersion 22-6-97 | 7:34:49 pm 'ACV'*"!

change: elValor

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
	
	unValor = change  ifTrue: [ ^self].

	change := unValor.
	self markDirty.
	self changed: #change!

initialSelection
	^initialSelection
"*VIPVersion 22-6-97 | 7:34:49 pm 'ACV'*"!

initialSelection: elValor

	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ 
					unosStrings size > 1
						ifTrue: [ unosStrings first asString]
						ifFalse: [ elValor asString]
				]
		].
	
	unValor = initialSelection  ifTrue: [ ^self].

	initialSelection := unValor.
	self markDirty.
	self initialSelection: #initialSelection!

menu
	^menu
"*VIPVersion 22-6-97 | 7:34:49 pm 'ACV'*"!

menu: elValor

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
	
	unValor = menu  ifTrue: [ ^self].

	menu := unValor.
	self markDirty.
	self changed: #menu!

readOnly
	^readOnly
"*VIPVersion 22-6-97 | 7:34:50 pm 'ACV'*"!

readOnly: elValor

	| unValor unosStrings |
	unValor := elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
					(elValor isNil or: [ elValor isEmpty])
						ifTrue: [ false] 
						ifFalse: [ 	
							unosStrings := elValor  asArrayOfSubstrings.
							unosStrings isEmpty
								ifTrue: [ false]
								ifFalse: [ 
									unosStrings size > 1
										ifTrue: [ unosStrings first = true printString]
										ifFalse: [ elValor = true printString]
								]
						]
				]
		].
	
	unValor = readOnly  ifTrue: [ ^self].

	readOnly := unValor.
	self markDirty.
	self changed: #readOnly! !

!CMDefinedPartCode publicMethodsFor: 'ayuda'!

explicacion
	
	^'(Texto) ', self nombre, super explicacion
"*VIPVersion 12-7-97 | 1:55:02 am 'ACV'*"! !

!CMDefinedPartCode publicMethodsFor: 'initialize-release'!

aspect: elAspect change: elChange menu: elMenu initialSelection: elInitialSelection layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup 

	aspect := elAspect.
	change := elChange.
	menu := elMenu.
	initialSelection := elInitialSelection.
	layoutSymbol := elLayoutSymbol.
	layoutGroup := elLayoutGroup.
	^self
"*VIPVersion 22-6-97 | 7:34:50 pm 'ACV'*"!

initialize
	super initialize.

	aspect	:= nil.
	change	:= nil.
	menu	:= nil.
	initialSelection	:= nil.
	readOnly := nil.!

release
	aspect	:= nil.
	change	:= nil.
	menu	:= nil.
	initialSelection	:= nil.
	layoutSymbol	:= nil.
	layoutGroup	:= nil.
	readOnly := nil.
		super release

"*VIPVersion 22-6-97 | 7:34:50 pm 'ACV'*"! !

!CMDefinedPartCode publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	|  aLayout   anAdaptor  aTextEditorSpec aLookPreferences |

	theDefinedEditor isRemoteClient ifTrue: [ 
		^self remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder].
	theDefinedEditor isRemoteServer ifTrue: [ 
		^self remoteServerBuilderSpecFrom: theDefinedEditor with: theBuilder].

	anAdaptor := theDefinedEditor adaptorForCMDefinedPart: self  get: self aspect put: self change.
	anAdaptor isNil ifTrue: [
		anAdaptor := AspectAdaptor
			accessWith: self aspect 
			assignWith: self change.
		anAdaptor subject: theDefinedEditor.
		anAdaptor subjectSendsUpdates: true].

	aLayout := theDefinedEditor layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles.

	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.

 	aTextEditorSpec := PROTextEditorSpec new.

	aTextEditorSpec
		name: self name;
		layout: aLayout; 
		model: anAdaptor;
		menu: nil;
		isReadOnly: self selectorActivo = false;
		colors: aLookPreferences;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) .


	^aTextEditorSpec! !

!CMDefinedPartCode publicMethodsFor: 'persistence-code'!

initFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	self aspect: 				(theValues size < 13 ifTrue: [nil] ifFalse: [ theValues at: 13]). 
	self change: 				(theValues size < 14 ifTrue: [nil] ifFalse: [ theValues at: 14]).
	self readOnly:			(theValues size < 15 ifTrue: [nil] ifFalse: [ theValues at: 15]).
	self menu: 				(theValues size < 16 ifTrue: [nil] ifFalse: [ theValues at: 16]). 
	self initialSelection: 	(theValues size < 17 ifTrue: [nil] ifFalse: [ theValues at: 17]).!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self aspect); nextPutAll: aSep;  nextPutAll: (self pcForV:  self change); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self readOnly);  cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self menu); nextPutAll: aSep;  nextPutAll: (self pcForV:  self initialSelection); cr! !

!CMDefinedPartCode publicMethodsFor: 'save defined parts'!

innerSaveForVisualOnStream: elStream
	super innerSaveForVisualOnStream: elStream.
	elStream nextPutAll: 
		self aspect printString, '	',
		self change printString, '	',
		self menu printString, '	',
		self initialSelection printString, '	',
		self layoutSymbol printString, '	',
		self layoutGroup printString, '	',
		self readOnly printString, '	'

"*VIPVersion 22-6-97 | 7:34:50 pm 'ACV'*"! !

!CMDefinedPartColumn class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Column'!

kind
	^#column! !

!CMDefinedPartColumn class publicMethodsFor: 'instance creation'!

newFromLayoutRectangle: theLayoutRectangle
	
	| aName aColumnPart |
	theLayoutRectangle isNil ifTrue: [ ^nil].

	aName := self layoutNameFrom: theLayoutRectangle.
	aName isNil ifTrue: [ ^self].

	aColumnPart := self named: aName.
	aColumnPart forzeLayoutRectangle: theLayoutRectangle.
	^aColumnPart! !

!CMDefinedPartColumn class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^false! !

!CMDefinedPartColumn class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesColumn!

metaPerspectivesColumn
	^OrderedCollection new

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsColumn!

metaSelectorsColumn
	
	"METAChildSpecAutoViewEditor openOn: CMDefinedPartSep selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.
	^(OrderedCollection new: 9)
		yourself!

metaSelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartColumn selector: #metaSelectorsGeneral target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 5)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Name';
			basicSelector: #name;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Name';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Kind';
			basicSelector: #kind;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Kind';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Explicacion';
			basicSelector: #explicacion;
			type: #Text;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Explicacion';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'IsDirty';
			basicSelector: #isDirty;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'IsDirty';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DoPersistIfDirty';
			basicSelector: #doPersistIfDirty;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DoPersistIfDirty';
			displaySelector: nil;
			canShowInTree: false;
			yourself);
		yourself!

metaSelectorsWithLayoutRectangle

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartColumn selector: #metaSelectorsWithLayoutRectangle target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 10)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LayoutSymbol';
			basicSelector: #layoutSymbol;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LayoutSymbol';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LayoutGroup';
			basicSelector: #layoutGroup;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LayoutGroup';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RelativeHeight';
			basicSelector: #relativeHeight;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RelativeHeight';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MinExtentX';
			basicSelector: #minExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentX';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MinExtentY';
			basicSelector: #minExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentY';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MaxExtentX';
			basicSelector: #maxExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentX';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MaxExtentY';
			basicSelector: #maxExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentY';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RelXStart';
			basicSelector: #relXStart;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RelXStart';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RelXWidth';
			basicSelector: #relXWidth;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RelXWidth';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MustAdvanceRow';
			basicSelector: #mustAdvanceRow;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MustAdvanceRow';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		yourself! !

!CMDefinedPartColumn publicMethodsFor: 'accessing'!

layoutGroup
	self container isNil ifTrue: [ ^nil].
	^self container columnsLayoutGroup!

layoutGroup: elValor
	self shouldNotImplement!

layoutSymbol
	^self name!

layoutSymbol: theValue
	self name: theValue!

name: elNombre
	
	super name: elNombre.
	self class layoutName: elNombre into: self layoutRectangle.! !

!CMDefinedPartColumn publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles
	^nil! !

!CMDefinedPartField class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Campo'
"*VIPVersion 22-6-97 | 7:35:14 pm 'ACV'*"!

kind
	^#field
"*VIPVersion 22-6-97 | 7:35:14 pm 'ACV'*"! !

!CMDefinedPartField class publicMethodsFor: 'instance creation'!

named: 								elNombre
	visibilityPredicate:				theVisibilityPredicate
	initiallyVisiblePredicate:  	theInitiallyVisiblePredicate
	enablementPredicate: 		theEnablementPredicate 
	initiallyEnabledPredicate:	theInitiallyEnabledPredicate
	layoutMode:					theLayoutMode
	layoutSymbol:					theLayoutSymbol
	layoutGroup:					theLayoutGroup
	layoutRectangle:				theLayoutRectangle
	layoutFrame: 					theLayoutFrame
	aspect: 							theAspect
	change: 							theChange
	metaInfo:						theMetaInfo
	valueExpression:				theValueExpression
	menu: 							theMenu
	initialSelection: 				theInitialSelection
	enhance: 						theEnhance 
	justification:					theJustification
	borderWidth:					theBorderWidth

	| aDefinedPart |

	aDefinedPart := super named: elNombre.
	aDefinedPart
		visibilityPredicate:				theVisibilityPredicate;
		initiallyVisiblePredicate:  		theInitiallyVisiblePredicate;
		enablementPredicate: 			theEnablementPredicate ;
		initiallyEnabledPredicate:		theInitiallyEnabledPredicate;
		layoutMode:						theLayoutMode;
		layoutSymbol:					theLayoutSymbol;
		layoutGroup:						theLayoutGroup;
		layoutRectangle:				theLayoutRectangle;
		layoutFrame: 					theLayoutFrame;
		aspect: 							theAspect;
		change: 							theChange;
		metaInfo:							theMetaInfo;
		valueExpression:				theValueExpression;
		menu: 							theMenu;
		initialSelection:					theInitialSelection;
		enhance:							theEnhance;
		justification:						theJustification;
		borderWidth:					theBorderWidth.

	^aDefinedPart! !

!CMDefinedPartField class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartField class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesField!

metaPerspectivesField
	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Aspect' 'Change' 'Menu' 'InitialSelection' 'MetaInfo' 'ValueExpression' 'Enhance' 'Justification' 'BorderWidth')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartField_Perspectives';
			nlsItem: 'Specific';
			nlsTranslation: 'Especificos de Campo';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'MetaInfo'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('MetaInfo' )))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartField_Perspectives';
			nlsItem: 'MetaInfo';
			nlsTranslation: 'MetaInformacion';
			yourself);
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsField!

metaSelectorsField

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartField selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 13)
		add: (METATerminalChildSpec new
			name: 'Aspect';
			basicSelector: #aspect;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Aspect';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'Aspect';
			nlsTranslation: 'OperacionLectura';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Change';
			basicSelector: #change;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Change';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'Change';
			nlsTranslation: 'OperacionEscritura';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Menu';
			basicSelector: #menu;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Menu';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'Menu';
			nlsTranslation: 'Menu';
			yourself);
		add: (METATerminalChildSpec new
			name: 'InitialSelection';
			basicSelector: #initialSelection;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'InitialSelection';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'InitialSelection';
			nlsTranslation: 'SeleccionInicial';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Enhance';
			basicSelector: #enhance;
			type: #Enum;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Enhance';
			displaySelector: nil;
			enumValuesString: 'normal bold italic underline serif sansserif bolditalic boldunderline boldunderlineitalic underlineitalic';
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'Enhance';
			nlsTranslation: 'EnfasisLetras';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Justification';
			basicSelector: #justification;
			type: #Enum;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Justification';
			displaySelector: nil;
			enumValuesString: 'left right center';
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'Justification';
			nlsTranslation: 'Justificacion';
			yourself);
		add: (METATerminalChildSpec new
			name: 'BorderWidth';
			basicSelector: #borderWidth;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'BorderWidth';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'BorderWidth';
			nlsTranslation: 'GrosorBorde';
			yourself);
		add: (METATerminalChildSpec new
			name: 'ValueExpression';
			basicSelector: #valueExpression;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ValueExpression';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'ValueExpression';
			nlsTranslation: 'ExpresionDeValor';
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MetaInfo';
			basicSelector: #metaInfo;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Select;
			helpString: 'MetaInfo';
			displaySelector: nil;
			objectClassName: #CODEElement;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			nlsApp: 'CODE';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'MetaInfo';
			nlsTranslation: 'MetaInformacion';
			yourself);
		yourself! !

!CMDefinedPartField class publicMethodsFor: 'persistence-code'!

sizeSymbolAtValue: theValue
	theValue = 30 ifTrue: [ ^#minFieldHeight].
	theValue = 40 ifTrue: [ ^#midFieldHeight].
	theValue = 50 ifTrue: [ ^#bigFieldHeight].
	^nil!

sizeValueAtSymbol: theSymbol
	theSymbol = #minFieldHeight ifTrue: [ ^30].
	theSymbol = #midFieldHeight ifTrue: [ ^40].
	theSymbol = #bigFieldHeight ifTrue: [ ^50].
	^nil! !

!CMDefinedPartField publicMethodsFor: 'accessing'!

aspect
	^aspect!

aspect: elValor

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
	
	unValor = aspect  ifTrue: [ ^self].

	aspect := unValor.
	self markDirty.
	self changed: #aspect!

borderWidth
	^borderWidth
"*VIPVersion 22-6-97 | 7:35:08 pm 'ACV'*"!

borderWidth: elValor

	| unValor unosStrings |
	unValor := (elValor isKindOf: Number)
		ifTrue: [ elValor]
		ifFalse: [ 
			(elValor isNil or: [ elValor isEmpty])
				ifTrue: [ false] 
				ifFalse: [ 	
					unosStrings := elValor  asArrayOfSubstrings.
					unosStrings isEmpty
						ifTrue: [  0]
						ifFalse: [ 
							unosStrings size > 1 
								ifTrue: [ Number readFrom: unosStrings first readStream]
								ifFalse: [ Number readFrom:  elValor]
						]
				]
		].
	
	unValor = borderWidth  ifTrue: [ ^self].

	borderWidth := unValor.
	self markDirty.
	self changed: #borderWidth!

change
	^change!

change: elValor

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
	
	unValor = change  ifTrue: [ ^self].

	change := unValor.
	self markDirty.
	self changed: #change!

enhance
	^enhance
"*VIPVersion 22-6-97 | 7:35:08 pm 'ACV'*"!

enhance: elValor

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
	
	unValor = enhance  ifTrue: [ ^self].

	enhance := unValor.
	self markDirty.
	self changed: #enhance!

forzeMetaInfo: theMetaInfo

	metaInfo := theMetaInfo.!

initialSelection
	^initialSelection!

initialSelection: elValor

	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ 
					unosStrings size > 1
						ifTrue: [ unosStrings first asString]
						ifFalse: [ elValor asString]
				]
		].
	
	unValor = initialSelection  ifTrue: [ ^self].

	initialSelection := unValor.
	self markDirty.
	self changed: #initialSelection!

justification
	^justification
"*VIPVersion 22-6-97 | 7:35:08 pm 'ACV'*"!

justification: elValor

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
	
	unValor = justification  ifTrue: [ ^self].

	justification := unValor.
	self markDirty.
	self changed: #justification!

menu
	^menu!

menu: elValor

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
	
	unValor = menu  ifTrue: [ ^self].

	menu := unValor.
	self markDirty.
	self changed: #menu!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo

	(theMetaInfo isKindOf: Array) ifTrue: [ 
		metaInfoRefTmpValues := theMetaInfo.
		^self
	].

	metaInfo := theMetaInfo.

	self markDirty.
	self changed: #metaInfo!

valueExpression
	^valueExpression!

valueExpression: elValor

	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ elValor]
		].
	
	unValor = valueExpression  ifTrue: [ ^self].

	valueExpression := unValor.
	self markDirty.
	self changed: #valueExpression! !

!CMDefinedPartField publicMethodsFor: 'adaptors'!

buildValueAdaptorFrom: theDefinedEditor

	| anAdaptor aMetaInfo aValueExpression |

	aMetaInfo := self metaInfo.
	aMetaInfo isNil ifFalse: [ 
		^self class preferredCMMetaInfoAspectAdaptorClass newWithMetaInfo: self metaInfo
	].

	aValueExpression := self valueExpression.	
	(aValueExpression isNil or: [ aValueExpression isEmpty]) ifTrue: [ ^nil].

	anAdaptor := self class preferredCMValueExpressionAspectAdaptorClass newWithExpression: aValueExpression.
	^anAdaptor!

valueAdaptorFrom: theDefinedEditor

	| anAdaptor |

	theDefinedEditor isNil ifTrue: [ ^nil].

	anAdaptor := theDefinedEditor valueAdaptorForPart: self.
	^anAdaptor! !

!CMDefinedPartField publicMethodsFor: 'derived accessing'!

containerAllowedTypes
	| aContainer someAllowedTypes |
	aContainer := self container.
	aContainer isNil ifTrue: [ ^nil].

	someAllowedTypes := aContainer allowedTypesForSubPart.
	^someAllowedTypes! !

!CMDefinedPartField publicMethodsFor: 'initialize-release'!

initialize
	super initialize.
	aspect	:= nil.
	change	:= nil.
	menu	:= nil.
	initialSelection	:= nil.!

release
	aspect	:= nil.
	change	:= nil.
	menu	:= nil.
	initialSelection	:= nil.
	layoutSymbol	:= nil.
	layoutGroup	:= nil.
		super release

"*VIPVersion 22-6-97 | 7:35:13 pm 'ACV'*"! !

!CMDefinedPartField publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	|  aLayout    aFieldSpec aLookPreferences aVisibilityAdaptor aEnablementAdaptor aValueAdaptor |

	aLayout := self hasLayoutFrame
		ifFalse: [ 
			self hasCooperativeLayout
				ifTrue: [ theDefinedEditor layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles]
				ifFalse: [ nil]
		]
		ifTrue: [ self layoutFrame ].

	aLayout isNil ifTrue: [ ^nil].

	aVisibilityAdaptor 		:= self visibilityAdaptorFrom: theDefinedEditor.
	aEnablementAdaptor 	:= self enablementAdaptorFrom: theDefinedEditor.

	aValueAdaptor := self valueAdaptorFrom: theDefinedEditor.
	aValueAdaptor isNil ifTrue: [ ^nil].

	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.

	aFieldSpec :=  CMInputFieldSpec  new 
		name: self name;
		layout: aLayout; 
		model: aValueAdaptor;
		type: #string;
		menu: nil;
		isReadOnly: false;
		colors: aLookPreferences;
		definedPart: self;
		definedEditor: theDefinedEditor;
		visibilityAdaptor: 				aVisibilityAdaptor;
		initiallyVisible:  			  		(self evaluateConstantPredicate: self initiallyVisiblePredicate);
		enablementAdaptor: 			aEnablementAdaptor;
		initiallyEnabled:  				(self evaluateConstantPredicate: self initiallyEnabledPredicate).


	^aFieldSpec! !

!CMDefinedPartField publicMethodsFor: 'persistence-code'!

firstPersistenceIndexField
	^self numberPersistenceEntriesDefinedPart + 
		self numberPersistenceEntriesWithLayoutRectangle + 
		1!

forgetMetaInfo

	self metaInfo: nil.
	self changed: #metaInfo!

initFromValues: theValues
	
	| aFPI |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	aFPI := self firstPersistenceIndexField.

	self aspect: 					(theValues size < (aFPI + 0) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 0)]). 
	self change: 					(theValues size < (aFPI + 1) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 1)]).
	self menu: 					(theValues size < (aFPI + 2) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 2)]). 
	self initialSelection: 		(theValues size < (aFPI + 3) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 3)]).
	self enhance: 				(theValues size < (aFPI + 4) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 4)]).
	self justification: 			(theValues size < (aFPI + 5) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 5)]).
	self borderWidth: 			(theValues size < (aFPI + 6) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 6)]).
	self valueExpression: 		(theValues size < (aFPI + 7) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 7)]).

	theValues size < (aFPI + 8) ifFalse: [ self initReferencedMetaInfoFromValues: (theValues at: (aFPI + 8))]!

initMetaInfoRefTmpValuesFromModel 
	self metaInfo isNil ifTrue: [ ^self].

	metaInfoRefTmpValues := self metaInfo asReferenceArray!

initReferencedMetaInfoFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].

	(theValues first = CODEElement refToAttributeKindSymbol or: [ 
		theValues first = CODEElement refToAttributeKindSymbol  ]) ifFalse: [ ^nil].

	metaInfoRefTmpValues := theValues.!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep aMetaInfo |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  aspect); nextPutAll: aSep;  
		nextPutAll: (self pcForV:  self  change); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  menu); nextPutAll: aSep;  
		nextPutAll: (self pcForV:  self  initialSelection); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self enhance); nextPutAll: aSep;  
		nextPutAll: (self pcForV:  self justification); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self borderWidth); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  valueExpression); cr.

	aMetaInfo := self metaInfo.
	aMetaInfo isNil  
		ifTrue: [ theStream nextPutAll: anIS; nextPutAll: (self pcForV: nil)] 
		ifFalse: [ aMetaInfo asReferenceAsCodeStringOn:  theStream indent: anIS]. 
	
	theStream cr.!

rebindReferencedMetaInfoValuesFromSolver: theSolver
	
	| someElements |

	(metaInfoRefTmpValues isNil or: [ metaInfoRefTmpValues isEmpty]) ifTrue: [ ^self].

	someElements := CODEElement resolveReferencedElementFromPersistenceAsCode: metaInfoRefTmpValues  
		solver: theSolver.
	self metaInfo: someElements.
	metaInfoRefTmpValues := nil!

rebindToModelFromSolver: theSolver

	self rebindReferencedMetaInfoValuesFromSolver: theSolver.!

unbindFromModel

	self initMetaInfoRefTmpValuesFromModel.! !

!CMDefinedPartField publicMethodsFor: 'semantic links-custom'!

metaInfoLinkSelect
	| aModel aSelection someTypes someSelectableObjects someNavigableObjects |

	someTypes := self containerAllowedTypes.
	someTypes isNil 
		ifTrue: [ 
			someSelectableObjects := nil.
			someNavigableObjects := nil
		]
		ifFalse: [  
			someSelectableObjects := IdentitySet new: someTypes size * 3 + 1.
			someNavigableObjects := IdentitySet new: someTypes size * 3 + 1.
			someTypes do: [:aType | | someAttributes |
				someAttributes := aType allEffectiveAttributes.
				someSelectableObjects addAll: someAttributes.
				someNavigableObjects addAll: someAttributes.
				someNavigableObjects addAll: aType model allModules
			].
			someNavigableObjects addAll: someTypes.
		].
	
	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	
	[ 
		aSelection := aModel chooseWithPathAmong: someSelectableObjects navigable: someNavigableObjects.
		aSelection isNil ifTrue: [ ^nil].
		aSelection isAttribute
	] 
		whileFalse: 
	[ 
		Dialog warn: ('Por favor, seleccione un Atributo del modelo.\',
			'Usted ha seleccionado un ', aSelection nlsKind , 
			'\que no es aceptable como metaInfo de un Campo') withCRs.
	].
	
	self metaInfo: aSelection.

	^aSelection!

xmetaInfoLinkSelect
	| aSelectedType anInitialSelection aLowercaseAttributeName aMatchingType aMaxScore aSimilarType someContainerTypes someTypes aSelectedAttribute aModel |

	someContainerTypes := self containerAllowedTypes.
	
	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	someContainerTypes isEmpty ifTrue: [ 
		Dialog warn: ('No hay Tipos especificados por el contenedor del campo.\',
			'La metainformacion del campo corresponde a caracteristicas\',
			'observables en un Tipo asociado al contendor del campo',
			'Por favor asocie algun Tipo al contenedor del campo.\',
			'Y vuelva a intentar asociar metaInformacion a este campo.') withCRs.
		^self
	].

	someTypes := someContainerTypes asSortedCollection: [:a :b | a nlsName < b nlsName].

	anInitialSelection := self metaInfo.
	anInitialSelection isNil ifTrue: [ 
		aLowercaseAttributeName := self name asLowercase.
		aMatchingType := someTypes detect: [:aType | aType name asLowercase = aLowercaseAttributeName] ifNone: [ nil].
		aMatchingType isNil
			ifFalse: [ anInitialSelection := aMatchingType]
			ifTrue: [ 
				aMaxScore := -1.
				aSimilarType := nil.
				someTypes do: [:aType |   | aScore |
					aScore := aType name spellAgainst: aLowercaseAttributeName.
					aScore > aMaxScore ifTrue: [ aSimilarType := aType]
				].
				aSimilarType isNil ifFalse: [  aMatchingType := aSimilarType].
			]
	].
	aSelectedType := Dialog 
		choose: ('	Please, select a Type as ValueType of Attribute	\			', self name) withCRs
		fromList: (someTypes collect: [:aType | 
			Object errorSignal
				handle: [:anException | anException returnWith: aType name]
				do: [ aType nlsName, ' 	(', aType name, ')']]) 
		values: someTypes 
		lines: (((someTypes size + 1) max: 5) min: 18)
		cancel: [nil]
		initialSelection: anInitialSelection.
	aSelectedType isNil ifTrue: [ ^nil].





	self metaInfo: aSelectedAttribute.

	^aSelectedType!

xxmetaInfoLinkSelect
	| aSelectedType anInitialSelection aLowercaseAttributeName aMatchingType aMaxScore aSimilarType someContainerTypes someTypes aSelectedAttribute aModel aSelection |

	someContainerTypes := self containerAllowedTypes.
	
	aModel := self model.
	aModel isNil ifTrue: [ ^nil].
 	self halt.
	
	[ 
		aSelection := aModel chooseWithPath.
		aSelection isNil ifTrue: [ ^nil].
		aSelection isAttribute
	] 
		whileFalse: 
	[ 
		Dialog warn: ('Por favor, seleccione un Atributo del modelo.\',
			'Usted ha seleccionado un ', aSelection nlsKind , 
			'\que no es aceptable como metaInfo de un Campo') withCRs.
	].
	



	someContainerTypes isEmpty ifTrue: [ 
		Dialog warn: ('No hay Tipos especificados por el contenedor del campo.\',
			'La metainformacion del campo corresponde a caracteristicas\',
			'observables en un Tipo asociado al contendor del campo',
			'Por favor asocie algun Tipo al contenedor del campo.\',
			'Y vuelva a intentar asociar metaInformacion a este campo.') withCRs.
		^self
	].

	someTypes := someContainerTypes asSortedCollection: [:a :b | a nlsName < b nlsName].

	anInitialSelection := self metaInfo.
	anInitialSelection isNil ifTrue: [ 
		aLowercaseAttributeName := self name asLowercase.
		aMatchingType := someTypes detect: [:aType | aType name asLowercase = aLowercaseAttributeName] ifNone: [ nil].
		aMatchingType isNil
			ifFalse: [ anInitialSelection := aMatchingType]
			ifTrue: [ 
				aMaxScore := -1.
				aSimilarType := nil.
				someTypes do: [:aType |   | aScore |
					aScore := aType name spellAgainst: aLowercaseAttributeName.
					aScore > aMaxScore ifTrue: [ aSimilarType := aType]
				].
				aSimilarType isNil ifFalse: [  aMatchingType := aSimilarType].
			]
	].
	aSelectedType := Dialog 
		choose: ('	Please, select a Type as ValueType of Attribute	\			', self name) withCRs
		fromList: (someTypes collect: [:aType | 
			Object errorSignal
				handle: [:anException | anException returnWith: aType name]
				do: [ aType nlsName, ' 	(', aType name, ')']]) 
		values: someTypes 
		lines: (((someTypes size + 1) max: 5) min: 18)
		cancel: [nil]
		initialSelection: anInitialSelection.
	aSelectedType isNil ifTrue: [ ^nil].





	self metaInfo: aSelectedAttribute.

	^aSelectedType! !

!CMDefinedPartGroup class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Grupo'!

kind
	^#group! !

!CMDefinedPartGroup class publicMethodsFor: 'instance creation'!

multiplexedNamed: elNombre
	bordered:				elBordered
	subParts:					lasParts 
	layoutSymbol: 			elLayoutSymbol 
	layoutGroup: 			elLayoutGroup 
	selectorVisible:			elSelectorVisible
	valorVisible:				elValorVisible
	selectorVisual:			elSelectorVisual
	valorVisual:				elValorVisual
	selectorActivo:			elSelectorActivo
	valorActivo:				elValorActivo

	^(super named: elNombre) 
		bordered:					elBordered
		subParts:					lasParts 
		layoutSymbol: 			elLayoutSymbol 
		layoutGroup: 				elLayoutGroup;
		selectorVisible:			elSelectorVisible;
		valorVisible:				elValorVisible;
		selectorVisual:			elSelectorVisual;
		valorVisual:				elValorVisual;
		selectorActivo:			elSelectorActivo;
		valorActivo:				elValorActivo;
		multiplexed:				true;
		yourself
	
"*VIPVersion 22-6-97 | 7:34:47 pm 'ACV'*"!

multiplexedNamed: elNombre
	subParts:					lasParts 
	layoutSymbol: 			elLayoutSymbol 
	layoutGroup: 			elLayoutGroup 
	selectorVisible:			elSelectorVisible
	valorVisible:				elValorVisible
	selectorVisual:			elSelectorVisual
	valorVisual:				elValorVisual
	selectorActivo:			elSelectorActivo
	valorActivo:				elValorActivo

	^(super named: elNombre) 
		bordered:					true
		subParts:					lasParts 
		layoutSymbol: 			elLayoutSymbol 
		layoutGroup: 				elLayoutGroup;
		selectorVisible:			elSelectorVisible;
		valorVisible:				elValorVisible;
		selectorVisual:			elSelectorVisual;
		valorVisual:				elValorVisual;
		selectorActivo:			elSelectorActivo;
		valorActivo:				elValorActivo;
		multiplexed:				true;
		yourself
	
"*VIPVersion 22-6-97 | 7:34:47 pm 'ACV'*"!

named: elNombre
	bordered:				elBordered
	subParts:					lasParts 
	layoutSymbol: 			elLayoutSymbol 
	layoutGroup: 			elLayoutGroup 
	selectorVisible:			elSelectorVisible
	valorVisible:				elValorVisible
	selectorVisual:			elSelectorVisual
	valorVisual:				elValorVisual
	selectorActivo:			elSelectorActivo
	valorActivo:				elValorActivo

	^(super named: elNombre) 
		bordered:					elBordered
		subParts:					lasParts 
		layoutSymbol: 			elLayoutSymbol 
		layoutGroup: 				elLayoutGroup;
		selectorVisible:			elSelectorVisible;
		valorVisible:				elValorVisible;
		selectorVisual:			elSelectorVisual;
		valorVisual:				elValorVisual;
		selectorActivo:			elSelectorActivo;
		valorActivo:				elValorActivo;
		yourself
	
"*VIPVersion 22-6-97 | 7:34:47 pm 'ACV'*"!

named: elNombre
	subParts:					lasParts 
	layoutSymbol: 			elLayoutSymbol 
	layoutGroup: 			elLayoutGroup 
	selectorVisible:			elSelectorVisible
	valorVisible:				elValorVisible
	selectorVisual:			elSelectorVisual
	valorVisual:				elValorVisual
	selectorActivo:			elSelectorActivo
	valorActivo:				elValorActivo

	^(super named: elNombre) 
		bordered:					true
		subParts:					lasParts 
		layoutSymbol: 			elLayoutSymbol 
		layoutGroup: 				elLayoutGroup;
		selectorVisible:			elSelectorVisible;
		valorVisible:				elValorVisible;
		selectorVisual:			elSelectorVisual;
		valorVisual:				elValorVisual;
		selectorActivo:			elSelectorActivo;
		valorActivo:				elValorActivo;
		yourself
	
"*VIPVersion 22-6-97 | 7:34:47 pm 'ACV'*"! !

!CMDefinedPartGroup class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartGroup class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesGroup!

metaPerspectivesGroup
	^OrderedCollection new
		addLast: ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Bordered' 'Multiplexed' 'Aspect' 'StoreClassName' 'StoreMethodSelector')));
		addLast: ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'SubParts'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('SubParts')));

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsGroup!

metaSelectorsGroup

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartGroup selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 6)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Bordered';
			basicSelector: #bordered;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Bordered';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Multiplexed';
			basicSelector: #multiplexed;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Multiplexed';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Aspect';
			basicSelector: #aspect;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Aspect';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'StoreClassName';
			basicSelector: #storeClassName;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'StoreClassName';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'StoreMethodSelector';
			basicSelector: #storeMethodSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'StoreMethodSelector';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'SubParts';
			basicSelector: #subParts;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'SubParts';
			displaySelector: #name;
			canShowInTree: true;
			componentsClassName: #CMDefinedPart;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			yourself);
		yourself! !

!CMDefinedPartGroup publicMethodsFor: 'accessing'!

aspect
	^aspect!

aspect: elValor

	| unValor unosStrings |
	unValor :=  elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
	(elValor isNil or: [ elValor isEmpty])
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
		]]].
	
	unValor = aspect  ifTrue: [ ^self].

	aspect := unValor.
	self markDirty.
	self changed: #aspect!

bordered
	^bordered
"*VIPVersion 22-6-97 | 7:34:45 pm 'ACV'*"!

bordered: elValor

	| unValor unosStrings |
	unValor := elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
					(elValor isNil or: [ elValor isEmpty])
						ifTrue: [ false] 
						ifFalse: [ 	
							unosStrings := elValor  asArrayOfSubstrings.
							unosStrings isEmpty
								ifTrue: [ false]
								ifFalse: [ 
									unosStrings size > 1
										ifTrue: [ unosStrings first = true printString]
										ifFalse: [ elValor = true printString]
								]
						]
				]
		].
	
	unValor = bordered  ifTrue: [ ^self].

	bordered := unValor.
	self markDirty.
	self changed: #bordered!

multiplexed
	^multiplexed

"*VIPVersion 22-6-97 | 7:34:45 pm 'ACV'*"!

multiplexed: elValor

	| unValor unosStrings |
	unValor := elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
					(elValor isNil or: [ elValor isEmpty])
						ifTrue: [ false] 
						ifFalse: [ 	
							unosStrings := elValor  asArrayOfSubstrings.
							unosStrings isEmpty
								ifTrue: [ false]
								ifFalse: [ 
									unosStrings size > 1
										ifTrue: [ unosStrings first = true printString]
										ifFalse: [ elValor = true printString]
								]
						]
				]
		].
	
	unValor = multiplexed  ifTrue: [ ^self].

	multiplexed := unValor.
	self markDirty.
	self changed: #multiplexed!

storeClassName
	^storeClassName!

storeClassName: elValor

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
	
	unValor = storeClassName  ifTrue: [ ^self].

	storeClassName := unValor.
	self markDirty.
	self changed: #storeClassName!

storeMethodSelector
	^storeMethodSelector!

storeMethodSelector: elValor

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
	
	unValor = storeMethodSelector  ifTrue: [ ^self].

	storeMethodSelector := unValor.
	self markDirty.
	self changed: #storeMethodSelector! !

!CMDefinedPartGroup publicMethodsFor: 'ayuda'!

esPaso
	^self selectorVisible = #pasoActual 
"*VIPVersion 22-6-97 | 7:34:46 pm 'ACV'*"!

explicacion
	| unStream |
	self esPaso ifTrue: [ ^' en el Paso ', self nombre].

	^(selectorVisible isNil not or: [selectorActivo isNil not])  ifFalse: [ ''] ifTrue: [	
		unStream := WriteStream on: (String new: 32).
		(selectorVisible = selectorActivo and: [valorVisible = valorActivo]) 
			ifTrue: [ 
				unStream nextPutAll: ' Visible Y Modificable cuando ', selectorVisible,
					(valorVisible = true ifTrue: [ ''] ifFalse: [ ' sea igual a ', valorVisible printString]), '. '
			]
			ifFalse: [ 
				selectorVisible isNil not  ifTrue: [	
					unStream nextPutAll: ' Visible cuando ', selectorVisible,
						(valorVisible = true ifTrue: [ ''] ifFalse: [ ' sea igual a ', valorVisible printString]), '. '
				].
				selectorActivo isNil not  ifTrue: [	
					selectorActivo = false  
						ifTrue: [	 unStream nextPutAll: ' No Modificable. ' ]
						ifFalse: [ 
							unStream nextPutAll: ' Modificable cuando ', selectorActivo,
							(valorActivo = true ifTrue: [ ''] ifFalse: [ ' sea igual a ', valorActivo printString]), '. '
						]
				]
			].
		unStream contents
	]


"*VIPVersion 22-6-97 | 7:34:46 pm 'ACV'*"!

recursiveAllGroups
	
	| unasParts |
	unasParts := OrderedCollection new: 16.
	(self subParts select: [:unaPart | unaPart isKindOf: CMDefinedPartGroup]) do: [:unaPart |
		unasParts addLast: unaPart.
		unasParts addAllLast: (unaPart recursiveAllGroups)
	].	
	^unasParts

"*VIPVersion 22-6-97 | 7:34:46 pm 'ACV'*"!

recursiveDefinicionesAyudasSuper: elSuper nivel: elNivel
	|   unaDefinicionAyuda |

	unaDefinicionAyuda := DefinicionAyuda nuevaDefinicionAyuda: self nombre 
		tipo: DefinicionAyuda simboloTipoDefinicionAyudaCMDefinedPart.

	unaDefinicionAyuda labelListaDefinicionAyuda:  self stringParaListaAyudas.
	unaDefinicionAyuda nivelDefinicionAyuda:  elNivel.
	unaDefinicionAyuda elementoDefinicionAyuda:  self.
	unaDefinicionAyuda superDefinicionAyuda:  elSuper.
	unaDefinicionAyuda subsDefinicionAyuda:  (OrderedCollection new: self subParts size).
	unaDefinicionAyuda comentarioDefinicionAyuda:  ''.

	(self subParts select: [:unaPart | unaPart mostrarEnAyudaBrowser ]) do: [:unaPart |
		unaDefinicionAyuda subsDefinicionAyuda addLast: ((unaPart isKindOf: CMDefinedPartGroup) 
			ifTrue: [  unaPart recursiveDefinicionesAyudasSuper: unaDefinicionAyuda nivel: elNivel + 1 ]
			ifFalse: [  unaPart definicionAyudaSuper: unaDefinicionAyuda nivel: elNivel + 1]
		)
	].	

	^unaDefinicionAyuda

"*VIPVersion 22-6-97 | 7:34:46 pm 'ACV'*"!

recursivePartsFiltrando: elBloque level: elLevel
	| unasParts unasPartsPosibles otrasPartes  unSkipearGrupo unAddLevel unaExplicacion |

	 (elBloque value: self) ifTrue: [ 
		^self recursivePartsLevel: elLevel
	].

	unSkipearGrupo := self skipearGrupo.
	unAddLevel := unSkipearGrupo ifTrue: [ 0] ifFalse: [ 1].

	unasParts := OrderedCollection new: 16.
	unasPartsPosibles := self subParts select: [:unaPart | unaPart mostrarEnAyudaBrowser ].

	unasPartsPosibles do: [:unaPart |
		(unaPart isKindOf: CMDefinedPartGroup) 
			ifFalse: [ 
				(elBloque value: unaPart) ifTrue: [ 
					unasParts addLast: (Array  with: unaPart with: elLevel + unAddLevel 
						with: (Array with: unaPart explicacion))
				]
			]
			ifTrue: [ 
				otrasPartes := unaPart recursivePartsFiltrando: elBloque level: elLevel + unAddLevel.
				otrasPartes isEmpty ifFalse: [ 
					unasParts addAllLast: otrasPartes
				]
			]	
	].
	
	unaExplicacion := self explicacion.
	^unasParts isEmpty 
		ifTrue: [ unasParts]
		ifFalse: [
			unasParts := unasParts collect: [:unaP | 
				Array with: (unaP at: 1) with: (unaP at: 2) with: (Array with: unaExplicacion), (unaP at: 3)
			].
			unSkipearGrupo
				ifFalse: [  
					(OrderedCollection with: (Array with: self with: elLevel  with: 
						(Array with: unaExplicacion))) ,  unasParts
				]
				ifTrue: [  unasParts]
		]



"*VIPVersion 22-6-97 | 7:34:46 pm 'ACV'*"!

recursivePartsLevel: elLevel
	| unasParts  unSkipearGrupo unAddLevel unaExplicacion |


	unSkipearGrupo := self skipearGrupo.
	unAddLevel := unSkipearGrupo ifTrue: [ 0] ifFalse: [ 1].


	unasParts := OrderedCollection new: 16.
	(self subParts select: [:unaPart | unaPart mostrarEnAyudaBrowser ]) do: [:unaPart |
		(unaPart isKindOf: CMDefinedPartGroup) 
			ifFalse: [ 
				unasParts addLast: (Array with: unaPart with: elLevel + unAddLevel 
					with: (Array with: unaPart explicacion)).
			]
			ifTrue: [ 
				unasParts addAllLast: (unaPart recursivePartsLevel: elLevel + unAddLevel)
			]
	].	

	unaExplicacion := self explicacion.
	^unasParts isEmpty 
		ifTrue: [ unasParts]
		ifFalse: [
			unasParts := unasParts collect: [:unaP | 
				Array with: (unaP at: 1) with: (unaP at: 2) with: (Array with: unaExplicacion), (unaP at: 3)
			].
			unSkipearGrupo
				ifFalse: [  
					(OrderedCollection with: (Array with: self with: elLevel  with: 
						(Array with: unaExplicacion))) ,  unasParts
				]
				ifTrue: [  unasParts]
		]



"*VIPVersion 22-6-97 | 7:34:46 pm 'ACV'*"!

skipearGrupo
	| unNombre |
	unNombre := self nombre.

	^
	(unNombre indexOfSubCollection: 'Grupo Datos ' startingAt: 1) = 1 or: [ 
	(unNombre indexOfSubCollection: 'Grupo Fields ' startingAt: 1) = 1 or: [ 
	(unNombre indexOfSubCollection: 'Grupo Filtro ' startingAt: 1) = 1 or: [ 
	(unNombre indexOfSubCollection: 'Grupo Lista ' startingAt: 1) = 1 or: [ 
	(unNombre indexOfSubCollection: 'Grupo Listados ' startingAt: 1) = 1 or: [ 
	unNombre = 'Pasos'  or: [
	unNombre = 'Control'
	]]]]]]
	
"*VIPVersion 22-6-97 | 7:34:46 pm 'ACV'*"!

stringParaListaAyudas
	| unNombre |
	unNombre := self nombre.
	self selectorVisible = #pasoActual ifTrue: [ 
		^'Paso de ', unNombre
	].

	(unNombre indexOfSubCollection: 'Grupo Fields Filtro ' startingAt: 1) = 1 ifTrue: [ 
		^'Condiciones de Busqueda de ', (unNombre copyFrom: 'Grupo Fields Filtro ' size  to: unNombre size)
	].

	(unNombre indexOfSubCollection: 'Grupo Fields ' startingAt: 1) = 1 ifTrue: [ 
		^'Propiedades de ', (unNombre copyFrom: 'Grupo Fields ' size  to: unNombre size)
	].

	(unNombre indexOfSubCollection: 'Grupo Lista ' startingAt: 1) = 1 ifTrue: [ 
		^'Lista de ', (unNombre copyFrom: 'Grupo Lista ' size  to: unNombre size)
	].
	(unNombre indexOfSubCollection: 'Grupo Listados ' startingAt: 1) = 1 ifTrue: [ 
		^'Listados de ', (unNombre copyFrom: 'Grupo Listados ' size  to: unNombre size)
	].
	unNombre = 'Pasos'  ifTrue: [ 
		^'Pasos'
	].

	^unNombre
"*VIPVersion 22-6-97 | 7:34:47 pm 'ACV'*"! !

!CMDefinedPartGroup publicMethodsFor: 'boss'!

bossOut
	| aFileName anIndex |

	aFileName := self sourceFileName.
	(anIndex := aFileName indexOf: $.) > 0 ifTrue: [ aFileName := aFileName copyFrom: anIndex + 1 to: aFileName size].
	(aFileName isNil or: [ aFileName isEmpty]) ifTrue: [ aFileName := 'CMDefinedPartGroup'].
	aFileName := aFileName , '.bos'.
	^self bossOut: aFileName!

bossOut: theSourceFileName
	| unFilename aBoss aFileName anIndex |

	aFileName := theSourceFileName.
	(anIndex := aFileName indexOf: $.) > 0 ifTrue: [ aFileName := aFileName copyFrom: anIndex + 1 to: aFileName size].
	(aFileName isNil or: [ aFileName isEmpty]) ifTrue: [ aFileName := 'CMDefinedPartGroup'].
	unFilename := (aFileName , '.bos' ) asFilename.

	[
		aBoss := BinaryObjectStorage onNew: (unFilename withEncoding: #binary) writeStream.
		aBoss nextPut: self
	]
		valueNowOrOnUnwindDo: 
	[
		aBoss isNil ifFalse: [ aBoss close]
	]! !

!CMDefinedPartGroup publicMethodsFor: 'comparing'!

compareParts: theOther

	| anAddedCatalog aRemovedCatalog |
	theOther isNil ifTrue: [ ^self].

	anAddedCatalog := self class new.
	aRemovedCatalog := self class new.


	self partsPrivate do: [:onePart | |  otherPart |
		otherPart := theOther partNamed: onePart name.
		otherPart isNil 
			ifTrue: [  aRemovedCatalog partsAdd: onePart copyParts]
			ifFalse: [ 
				onePart compareParts: otherPart 
					added: anAddedCatalog  removed: aRemovedCatalog.
			]
	].

	theOther partsPrivate do: [:onePart | |  otherPart |
		otherPart := self partNamed: onePart name.
		otherPart isNil 
			ifTrue: [ anAddedCatalog partsAdd: onePart copyParts]
	].


	anAddedCatalog partsSize < 1 
		ifTrue: [ anAddedCatalog := nil ]
		ifFalse: [ 
			anAddedCatalog name: self name. 
			anAddedCatalog sourceFileName: 'added' copy].

	aRemovedCatalog partsSize < 1 
		ifTrue: [ aRemovedCatalog := nil]
		ifFalse: [ 
			aRemovedCatalog name: self name.
			aRemovedCatalog sourceFileName: 'removed' copy].

	^Array with: anAddedCatalog with: aRemovedCatalog!

compareSubParts: theOther

	| anAddedCatalog aRemovedCatalog |
	theOther isNil ifTrue: [ ^self].

	anAddedCatalog := self class new.
	aRemovedCatalog := self class new.


	self subPartsPrivate do: [:oneSubPart | |  otherSubPart |
		otherSubPart := theOther subPartNamed: oneSubPart name.
		otherSubPart isNil 
			ifTrue: [  aRemovedCatalog subPartsAdd: oneSubPart copySubParts]
			ifFalse: [ 
				oneSubPart compareSubParts: otherSubPart 
					added: anAddedCatalog  removed: aRemovedCatalog.
			]
	].

	theOther subPartsPrivate do: [:oneSubPart | |  otherSubPart |
		otherSubPart := self subPartNamed: oneSubPart name.
		otherSubPart isNil 
			ifTrue: [ anAddedCatalog subPartsAdd: oneSubPart copySubParts]
	].


	anAddedCatalog subPartsSize < 1 
		ifTrue: [ anAddedCatalog := nil ]
		ifFalse: [ 
			anAddedCatalog name: self name. 
			anAddedCatalog sourceFileName: 'added' copy].

	aRemovedCatalog subPartsSize < 1 
		ifTrue: [ aRemovedCatalog := nil]
		ifFalse: [ 
			aRemovedCatalog name: self name.
			aRemovedCatalog sourceFileName: 'removed' copy].

	^Array with: anAddedCatalog with: aRemovedCatalog! !

!CMDefinedPartGroup publicMethodsFor: 'copying'!

copyParts
	| aCopy  |
	aCopy := self class new.
	aCopy name: self name copy.
	aCopy language: self language  copy.
	aCopy sourceFileName: self sourceFileName  copy.

	self partsPrivate do: [:aPart |
		aCopy partsAdd: aPart copyParts].

	^aCopy!

copySubParts
	| aCopy  |
	aCopy := self class new.
	aCopy name: self name copy.
	aCopy language: self language  copy.
	aCopy sourceFileName: self sourceFileName  copy.

	self subPartsPrivate do: [:anApp |
		aCopy subPartsAdd: anApp copySubParts].

	self subPartsPrivate do: [:anApp | | aSuperApp aCopySuperApp |
		aSuperApp := anApp superSubPart.
		aSuperApp isNil ifFalse: [ 
			aCopySuperApp := aCopy subPartNamed: aSuperApp name.
			(aCopy subPartNamed: anApp name) superSubPart: aCopySuperApp]].

	^aCopy! !

!CMDefinedPartGroup publicMethodsFor: 'derived accessing'!

subGroups
	^self subPartsPrivate select: [:aPart | aPart isGroup]! !

!CMDefinedPartGroup publicMethodsFor: 'dirty'!

cleanDirtyMark
	super cleanDirtyMark.
	self subParts do: [:aPart |  aPart cleanDirtyMark]!

markDirty
	dirty == true ifFalse: [ 
		dirty := true.
		self changed: #dirty
	].
	self mustStore ifFalse: [ 
		self container isNil ifFalse: [ self container markDirtyContainer]].!

markDirtyContainer
	self mustStore 
		ifTrue: [ self markDirty]
		ifFalse: [ self container isNil ifFalse: [ self container markDirty]]! !

!CMDefinedPartGroup publicMethodsFor: 'initialize-release'!

bordered:		elBordered
	subParts:				lasParts 
	layoutSymbol: 	elLayoutSymbol 
	layoutGroup: 		elLayoutGroup
	
	bordered		:= elBordered.
	self subParts: lasParts.
	layoutSymbol	:= elLayoutSymbol.
	layoutGroup 	:= elLayoutGroup.
	multiplexed	:= false.


"*VIPVersion 22-6-97 | 7:34:44 pm 'ACV'*"!

initialize
	super initialize.

	bordered := nil.

	multiplexed := nil.
	aspect := nil.
	storeMethodSelector := nil.
	storeClassName := nil.
	bordered := false.
	layoutRectangles := Array new.
	subParts := nil.!

release
	"Generated by ISF/AD. Do not modify"

	bordered := nil.
	multiplexed := nil.
	aspect := nil.
	storeClassName := nil.
	storeMethodSelector := nil	.

	self subPartsRelease.
	subParts := nil.

	super release! !

!CMDefinedPartGroup publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	
	 | someSpecs aCompositeSpecCol aCompositeSpec aLayout unModel anAspect |

	theDefinedEditor isRemoteClient ifTrue: [ 
		^self remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].
	theDefinedEditor isRemoteServer ifTrue: [ 
		^self remoteServerBuilderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	self multiplexed
		ifFalse: [ unModel := theDefinedEditor]
		 ifTrue: [
			anAspect := self aspect isNil ifTrue: [ self valorActivo] ifFalse: [ self aspect].
			unModel :=  MultiplexedModel on: theDefinedEditor aspect: anAspect.
			theDefinedEditor addMultiplexedModel: unModel.
			"theDefinedEditor addDependent: unModel"].

	someSpecs := OrderedCollection new.
	self subParts do: [:unaCMDefinedPart | | aSpec |
		aSpec := unaCMDefinedPart builderSpecFrom: unModel with: theBuilder withDefaultLayouts: self layoutRectangles.
		aSpec isNil ifFalse: [ someSpecs add: aSpec]
	].
	
	aLayout := theDefinedEditor layoutFor: self layoutSymbol  in: self layoutGroup withDefaultLayouts: theLayoutRectangles.

	aCompositeSpecCol := CompositeSpecCollection new. 
	aCompositeSpecCol collection: someSpecs asArray.
	aCompositeSpec := PROCompositeSpec new.
	aCompositeSpec
		name: self name;
		layout: aLayout;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) ;
		definedPart: self;
		definedEditor: theDefinedEditor.
	aCompositeSpecCol compositeSpec: aCompositeSpec.

	^aCompositeSpecCol
"*VIPVersion 10-7-97 | 3:20:35 am 'ACV'*"!

remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles
	
	 | someSpecs aCompositeSpecCol aCompositeSpec aLayout unModel aRemoteEditor |


	(theDefinedEditor isRemoteClient not or: [ self forceLocal]) ifTrue: [ 
		^self builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	aRemoteEditor := theDefinedEditor remoteEditorModel.
	aRemoteEditor isNil ifTrue: [ 
		^self builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	self multiplexed
		ifFalse: [ unModel := theDefinedEditor]
		 ifTrue: [
			unModel :=  RemoteMultiplexedModel on: theDefinedEditor aspect: self aspect 
				remoteEditor: aRemoteEditor.
			theDefinedEditor addMultiplexedModel: unModel.
			"theDefinedEditor addDependent: unModel"].


	someSpecs := OrderedCollection new.
	self subParts do: [:unaCMDefinedPart | | aSpec |
		aSpec := unaCMDefinedPart remoteClientBuilderSpecFrom: unModel with: theBuilder.
		aSpec isNil ifFalse: [ someSpecs add: aSpec]].
	
	aLayout := theDefinedEditor  layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles.

	aCompositeSpecCol := CompositeSpecCollection new. 
	aCompositeSpecCol collection: someSpecs asArray.
	aCompositeSpec := PRORemoteCompositeSpec new.
	aCompositeSpec
		name: self name;
		layout: aLayout;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) ;
		definedPart: self;
		definedEditor: aRemoteEditor.
	aCompositeSpecCol compositeSpec: aCompositeSpec.

	^aCompositeSpecCol! !

!CMDefinedPartGroup publicMethodsFor: 'layout'!

initializeLayoutsFromEditor: theEditor
	| someParts aLayoutGroup someLayouts someReorganizedParts |
	theEditor isNil  ifTrue: [ ^self].

	someParts := self subParts.
	(someParts isNil or: [ someParts isEmpty]) ifTrue: [ ^self].

	self testIfAllSubPartsHaveSameLayoutGroups  ifFalse: [ 
 Transcript show: self kind , ' ',  self name , ' can not initializeLayoutsFromEditor because subParts have more than one LayoutGroup'; cr.
		someParts do: [:aPart | aPart isGroup ifTrue: [ aPart  initializeLayoutsFromEditor: theEditor]].
		^self
	].
	
	aLayoutGroup := someParts first layoutGroup.
	someLayouts := theEditor defaultLayoutsFor: aLayoutGroup.
	(someLayouts isNil or: [someLayouts isEmpty]) ifTrue: [ ^self].

	layoutRectangles := someLayouts.

	someReorganizedParts := OrderedCollection new: someParts size * 2.

	someLayouts do: [:aLayout |   | aLayoutSymbol aPart |
		aLayoutSymbol := self class layoutSymbolFromLCA: aLayout.
		aLayoutSymbol isNil ifFalse: [ 
			aLayoutSymbol = #sep
				ifTrue: [
					aPart := CMDefinedPartSep named: 'sep '.
					aPart layoutSymbol: aLayoutSymbol.
					aPart layoutGroup: aLayoutGroup.	
					aPart initLayoutRectangleFromIdentical: aLayout.
					aPart containerPrivate: self.
					someReorganizedParts add: aPart			
				]
				ifFalse: [ 
					aPart := self subPartWithLayoutSymbol: aLayoutSymbol.
					aPart isNil 
						ifFalse:  [ 
							aPart initLayoutRectangleFromIdentical: aLayout.
							someReorganizedParts add: aPart.
							aPart isMultiList ifTrue: [ aPart initializeLayoutsFromEditor: theEditor]
						]
						ifTrue: [ 
							aPart := CMDefinedPartAnonymous named: 'anon ', aLayoutSymbol.
							aPart layoutSymbol: aLayoutSymbol.
							aPart layoutGroup: aLayoutGroup.
							aPart initLayoutRectangleFromIdentical: aLayout.
							aPart containerPrivate: self.
							someReorganizedParts add: aPart
						]
				]
		]
	].
	someParts do: [:aPart |
		((aPart isKindOf: CMDefinedPartSep) or: [ aPart isKindOf: CMDefinedPartAnonymous]) ifFalse: [ 
			(someReorganizedParts includes: aPart) ifFalse: [ 
				someReorganizedParts add: aPart
			]
		]
	].

	self subPartsPrivate: someReorganizedParts.

	someParts do: [:aPart | aPart isGroup ifTrue: [ aPart  initializeLayoutsFromEditor: theEditor]].!

layoutRectangles
	^layoutRectangles!

registerNewLayoutRectangle: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [ ^self].

	layoutRectangles := layoutRectangles , (Array with: theLayoutRectangle)!

reorderLayoutRectangles
	| someParts someLayoutRectangles  |
	someParts := self subParts.
	(someParts isNil or: [ someParts isEmpty]) ifTrue: [ ^self].

	someLayoutRectangles := OrderedCollection new: self subPartsSize.
	self subPartsPrivate do: [:aPart | | aLayoutRectangle |
		aPart hasLayoutRectangle ifTrue: [ 
			aLayoutRectangle := aPart layoutRectangle.
			aLayoutRectangle isNil ifFalse: [ someLayoutRectangles add: aLayoutRectangle]]].

	layoutRectangles := someLayoutRectangles! !

!CMDefinedPartGroup publicMethodsFor: 'persistence-code'!

initFromValues: theValues
	
	| someLayoutRectangles |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	self aspect: 				(theValues size < 13 ifTrue: [nil] ifFalse: [ theValues at: 13]). 
	self multiplexed: 			(theValues size < 14 ifTrue: [nil] ifFalse: [ theValues at: 14]).
	self bordered: 			(theValues size < 15 ifTrue: [nil] ifFalse: [ theValues at: 15]). 
	self storeClassName: 	(theValues size < 16 ifTrue: [nil] ifFalse: [ theValues at: 16]). 
	self storeMethodSelector: 	(theValues size < 17 ifTrue: [nil] ifFalse: [ theValues at: 17]). 
	self subParts: 			(theValues size < 18 ifTrue: [nil] ifFalse: [
		CMDefinedPart newSubPartsCollectionFromPersistenceAsCode: (theValues at: 18)]).

	someLayoutRectangles := OrderedCollection new: self subPartsSize.
	self subPartsPrivate collect: [:aPart | | aLayout | 
		aPart hasLayoutRectangle ifTrue: [ 
			aLayout := aPart layoutRectangle.
			aLayout isNil ifFalse: [ someLayoutRectangles add: aLayout]
		]
	].

	layoutRectangles := someLayoutRectangles asArray!

inner: theIsInner persistenceAsCodeStringChunks: theCollection on: theStream filter: theFilter indent: theIS doSubChunks: theDoSubChunks

	| aSep aNewStream aNewChunk |

	aSep := self separatorForPersistenceAsCode.
		
	aNewStream :=  nil.
	aNewChunk := nil.

	theIsInner
		ifTrue: [ 
			self mustStore 
				ifFalse: [ 
					theStream isNil ifFalse: [ 
						theStream nextPutAll: theIS; nextPutAll: '( ';  nextPutAll: (self pcForV: self kind);  
							nextPutAll: aSep; nextPutAll: self name printString; cr].
					self localValuesPersistenceAsCodeStringChunks: theCollection 
						on: theStream filter: theFilter indent: theIS doSubChunks: theDoSubChunks.
					theStream isNil ifFalse: [ theStream	nextPutAll: theIS; nextPutAll: ' )' ; cr; cr]
				]
				ifTrue: [ 
					self persistenceRefToMethodAsCodeStringOn: theStream indent: theIS.
					theDoSubChunks ifTrue: [ 

						(self mustStoreWithFilter: theFilter) ifTrue: [ 
							aNewStream := WriteStream on: (String new: self subPartsSize * 256).
							aNewChunk := Array with: self with: aNewStream.
							theCollection add: aNewChunk
						].
						self inner: false persistenceAsCodeStringChunks: theCollection 
							on: aNewStream filter: theFilter indent: self indentStringForPersistenceAsCode 
							doSubChunks: theDoSubChunks
					]
				]
		]
		ifFalse: [ 
			theStream isNil ifFalse: [ 
				theStream nextPutAll: theIS; nextPutAll: '#( ';  nextPutAll: (self pcForV: self kind);  
					nextPutAll: aSep; nextPutAll: self name printString; cr].
			self localValuesPersistenceAsCodeStringChunks: theCollection 
				on: theStream filter: theFilter indent: theIS doSubChunks: theDoSubChunks.
			theStream isNil ifFalse: [ theStream	nextPutAll: theIS; nextPutAll: ' )' ; cr; cr]
		]!

localValuesPersistenceAsCodeStringChunks: theCollection on: theStream filter: theFilter indent: theIS doSubChunks: theDoSubChunks

	| anIS aSep |

	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	theStream isNil ifFalse: [ 
		super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

		theStream
			nextPutAll: anIS;  nextPutAll: (self pcForV:  self aspect); nextPutAll: aSep;  nextPutAll: (self pcForV:  self multiplexed); cr;
		     nextPutAll: anIS;  nextPutAll: (self pcForV:  self bordered ); cr;
		     nextPutAll: anIS;  nextPutAll: (self pcForV:  self storeClassName );  nextPutAll: aSep; nextPutAll: (self pcForV:  self storeMethodSelector ); cr;
	     	nextPutAll: anIS;  nextPutAll: '('; nextPutAll: (self pcForV:  self class subPartsPersistenceSymbol); cr; cr
	].

	self subParts do:  [:aPart |
		aPart isGroup
			ifFalse: [ 
				aPart persistenceAsCodeStringOn: theStream indent: anIS , self indentStringForPersistenceAsCode
			]
			ifTrue: [ 
				aPart inner: true persistenceAsCodeStringChunks: theCollection on: theStream 
					filter: theFilter indent: anIS , self indentStringForPersistenceAsCode doSubChunks: theDoSubChunks
			]
		].
	
	theStream isNil ifFalse: [  theStream nextPutAll: anIS; nextPutAll: ' )' ; cr; cr]!

mustStore

	^(self multiplexed == true and: [ 
		self storeMethodSelector isNil not and: [ 
			self storeMethodSelector isEmpty not and: [ 
				self storeClassName isNil not and: [ 
					self storeClassName isEmpty not]]]])!

mustStoreWithFilter: theFilter

	^self mustStore and: [
		theFilter = true or: [ 
			(theFilter = #dirty and: [ self isDirty]) or: [ 
				(self preferredInstallerClass existsGroupMethod: self storeMethodSelector inClass: self storeClassName) not or: [ 
					theFilter = #source and: [
						(self preferredInstallerClass sameSource: self persistenceAsCodeString
							groupMethod: self storeMethodSelector asSymbol inClass: self storeClassName asSymbol) not
					]
				]
			]
		]
	]!

persistenceAsCodeString

	| aStream aCollection aChunk |


	aStream := WriteStream on: (String new: (self subPartsSize + 1)* 256).
	aCollection := OrderedCollection new: 32.
	aChunk := Array with: self with: aStream.
	aCollection add: aChunk.
	self inner: false persistenceAsCodeStringChunks: aCollection 
		on: aStream filter: true indent: self indentStringForPersistenceAsCode doSubChunks: false.

	^aStream contents!

persistenceRefToMethodAsCodeStringOn: theStream indent: theIS

	| aSep |
	theStream isNil ifTrue: [ ^self]. 

	aSep := self separatorForPersistenceAsCode.
		
	theStream nextPutAll: theIS; nextPutAll: '( ';  nextPutAll: (self pcForV: self class refToGroupMethodKind);  
		nextPutAll: aSep; nextPutAll: self name printString;  
		nextPutAll: aSep; nextPutAll: (self pcForV: self storeMethodSelector); nextPutAll: aSep; nextPutAll: (self pcForV: self storeClassName);  
		nextPutAll: ' )' ; cr; cr.! !

!CMDefinedPartGroup publicMethodsFor: 'remote'!

sampleRemoteValue
	^true! !

!CMDefinedPartGroup publicMethodsFor: 'save defined parts'!

innerSaveForVisualOnStream: elStream
	elStream nextPutAll: '$part	',
		self kind printString, '	[	',
		self name printString, '	',
		self selectorVisible printString, '	',
		self valorVisible printString, '	',
		self selectorVisual printString, '	',
		self valorVisual printString, '	',
		self selectorActivo printString, '	',
		self valorActivo printString, '	'.

	elStream nextPutAll: 
		self bordered  printString, '	',
		self layoutSymbol printString, '	',
		self layoutGroup printString, '	',
		self multiplexed printString, '	'

"*VIPVersion 22-6-97 | 7:34:47 pm 'ACV'*"! !

!CMDefinedPartGroup publicMethodsFor: 'subPart initialize-release'!

initSubParts
	"Generated by ISF/AD. Do not modify"
	subParts := OrderedCollection new.!

subPartsRelease
	"Generated by ISF/AD. Do not modify"
	self subParts do: [:each | self subPartsRemove: each]! !

!CMDefinedPartGroup publicMethodsFor: 'subParts accessing'!

subPartNamed: theName
	"Generated by ISF/AD. Do not modify"
	theName isNil ifTrue: [ ^nil].
	^self subPartsPrivate detect: [:anAT | anAT name = theName] ifNone: [nil]!

subParts
	"Generated by ISF/AD. Do not modify"
	^self subPartsPrivate copy!

subParts: lasParts
	self subPartsRelease.
	subParts := lasParts.
	subParts isNil ifFalse: [ 
		subParts do: [:aPart | aPart containerPrivate: self]]!

subPartsAsArray
	"Generated by ISF/AD. Do not modify"
	^self subParts asArray!

subPartWithLayoutSymbol: theLayoutSymbol
	theLayoutSymbol isNil ifTrue: [ ^nil].

	^self subParts detect: [:aPart | theLayoutSymbol = aPart layoutSymbol] ifNone: [ nil]! !

!CMDefinedPartGroup publicMethodsFor: 'subParts creation'!

subPartsLinkCreate
	
	| aMenu aFactory aPart anExistingNLSPart anExistingLayoutPart |
	aMenu := self class menuCMDefinedPartKinds.
	aMenu isNil ifTrue: [ ^nil].
	
	aFactory := aMenu startUp.
	aFactory isNil ifTrue: [ ^nil].
	aFactory == 0 ifTrue: [ ^nil].

	aPart := aFactory named: 'new ', aFactory kind.
	self subPartsAdd: aPart.
	self registerNewLayoutRectangle: aPart layoutRectangle.

	aPart isNLS ifTrue: [ 
		anExistingNLSPart := self subPartsPrivate detect: [:aDP | 
			aDP isNLS and: [ aDP nlsGroup isNil not and: [ aDP nlsGroup isEmpty not]]] ifNone: [ nil].
		anExistingNLSPart isNil ifFalse: [ aPart nlsGroup: anExistingNLSPart nlsGroup]].
	
	aPart hasLayoutRectangle ifTrue: [ 
		anExistingLayoutPart := self subPartsPrivate detect: [:aDP | 
			aDP hasLayoutRectangle and: [ aDP layoutGroup isNil not and: [ aDP layoutGroup isEmpty not]]] ifNone: [ nil].
		anExistingLayoutPart isNil ifFalse: [ 
			aPart layoutSymbol: (anExistingLayoutPart layoutSymbol , self subPartsSize printString) asSymbol.
			aPart layoutGroup: anExistingLayoutPart layoutGroup]].

	self changed: #subParts.
	
	^aPart! !

!CMDefinedPartGroup publicMethodsFor: 'subParts modifying'!

subPartsAdd: aValue
	"Generated by ISF/AD. Do not modify"
	(self subPartsIncludes: aValue) ifTrue: [^self subPartsMoveBottom: aValue].
	(self subPartsPrivateAdd: aValue) containerPrivate: self.
	self changed: #subParts.
	^aValue!

subPartsMoveBottom: aValue
	"Generated by ISF/AD. Do not modify"
	| aRes |
	(self subPartsIncludes: aValue) ifFalse: [^aValue].
	(self subParts indexOf: aValue) = self subPartsSize ifTrue: [^aValue].
	self subPartsPrivate remove: aValue.
	aRes := self subPartsPrivateAdd: aValue.
	self reorderLayoutRectangles.
	self changed: #subParts.
	^aRes!

subPartsMoveDown: aValue
	"Generated by ISF/AD. Do not modify"
	| index aRes |
	(self subPartsIncludes: aValue) ifFalse: [^aValue].
	(index := self subParts indexOf: aValue) = self subPartsSize ifTrue: [^aValue].
	index = (self subPartsSize -1)
		ifTrue:
			[self subPartsPrivate remove: aValue.
			aRes := self subPartsPrivateAdd: aValue.
			self reorderLayoutRectangles.
			^aRes].
	aRes := self subPartsPrivateMove: aValue beforeIndex: index + 2.
	self reorderLayoutRectangles.
	self changed: #subParts.
	^aRes!

subPartsMoveTop: aValue
	"Generated by ISF/AD. Do not modify"
	(self subPartsIncludes: aValue) ifFalse: [^aValue].
	(self subParts indexOf: aValue) = 1 ifTrue: [^aValue].
	self subPartsPrivateMove: aValue beforeIndex: 1.
	self reorderLayoutRectangles.
	self changed: #subParts.!

subPartsMoveUp: aValue
	"Generated by ISF/AD. Do not modify"
	| index aRes |
	(self subPartsIncludes: aValue) ifFalse: [^aValue].
	(index := self subParts indexOf: aValue) = 1 ifTrue: [^aValue].
	aRes := self subPartsPrivateMove: aValue beforeIndex: index - 1.

	self reorderLayoutRectangles.
	self changed: #subParts.
	^aRes!

subPartsRemove: aValue
	"Generated by ISF/AD. Do not modify"
	(self subPartsPrivate remove: aValue ifAbsent: [^aValue]) containerPrivate: nil.
	self reorderLayoutRectangles.
	self changed: #subParts.
	^aValue! !

!CMDefinedPartGroup publicMethodsFor: 'subParts private'!

subPartsPrivate
	"Generated by ISF/AD. Do not modify"
	subParts isNil
		ifTrue: [self initSubParts].
	^subParts!

subPartsPrivate: lasParts
	subParts := lasParts.!

subPartsPrivateAdd: aValue
	"Generated by ISF/AD. Do not modify"
	self subPartsPrivate add: aValue.
	self changed: #subParts.
	self markDirty.

	^aValue!

subPartsPrivateMove: aValue beforeIndex: anIndex
	"Generated by ISF/AD. Do not modify"
	| obj |
	obj := self subParts at: anIndex.
	self subPartsPrivate remove: aValue.
	self subPartsPrivate add: aValue before: obj.
	self markDirty.

	self changed: #subParts.
	^aValue!

subPartsPrivateRemove: aValue
	"Generated by ISF/AD. Do not modify"
	self subPartsPrivate remove: aValue.
	self markDirty.

	self changed: #subParts.
	^aValue! !

!CMDefinedPartGroup publicMethodsFor: 'subParts testing'!

subPartsIncludes: aValue
	"Generated by ISF/AD. Do not modify"
	^subParts isNil
		ifTrue: [false]
		ifFalse: [self subParts includes: aValue]!

subPartsSize
	"Generated by ISF/AD. Do not modify"
	^subParts isNil
		ifTrue: [0]
		 ifFalse: [subParts size]! !

!CMDefinedPartGroup publicMethodsFor: 'tests'!

isGroup
	^true!

reportAllSubPartsHaveSameLayoutGroups

	| aLayoutGroup |
	aLayoutGroup := nil.
	self subParts do: [:aPart |  | aRes |
		aLayoutGroup isNil 
			ifTrue: [ aLayoutGroup := aPart layoutGroup ]
			ifFalse: [   
				aLayoutGroup = aPart layoutGroup ifFalse: [ ^Array with: self with: aPart].
				aPart isGroup ifTrue: [ 
					aRes := aPart testIfAllSubPartsHaveSameLayoutGroups.
					aRes isNil ifFalse: [ ^aRes]]]].
	^nil!

testIfAllSubPartsHaveSameLayoutGroups

	| aLayoutGroup  |
	aLayoutGroup := nil.
	self subParts do: [:aPart | | |
		aLayoutGroup isNil 
			ifTrue: [ aLayoutGroup := aPart layoutGroup ]
			ifFalse: [ aLayoutGroup = aPart layoutGroup ifFalse: [ 
self halt.
Transcript show: 'SubParts NOT SAME LayoutGroup '; show: self name; show: ' container '; show: container name; cr.
			^false].
		aPart isGroup ifTrue: [ aPart  testIfAllSubPartsHaveSameLayoutGroups ifFalse: [ ^false]]]].
	^true! !

!CMDefinedPartImage class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Imagen'
"*VIPVersion 22-6-97 | 7:34:56 pm 'ACV'*"!

kind
	^#image
"*VIPVersion 22-6-97 | 7:34:57 pm 'ACV'*"! !

!CMDefinedPartImage class publicMethodsFor: 'instance creation'!

named: 					elNombre
	imageSelector: 		elSelectorImage
	layoutSymbol:		elLayoutSymbol
	layoutGroup:		elLayoutGroup
	borderWidth:		elBorderWidth
	selectorVisible:		elSelectorVisible
	valorVisible:			elValorVisible
	selectorVisual:		elSelectorVisual
	valorVisual:			elValorVisual
	selectorActivo:		elSelectorActivo
	valorActivo:			elValorActivo
	

	^(super named: elNombre)
		imageSelector:		elSelectorImage
		layoutSymbol:			elLayoutSymbol
		layoutGroup:			elLayoutGroup
		borderWidth:			elBorderWidth;
		selectorVisible:		elSelectorVisible;
		valorVisible:			elValorVisible;
		selectorVisual:		elSelectorVisual;
		valorVisual:			elValorVisual;
		selectorActivo:		elSelectorActivo;
		valorActivo:			elValorActivo;
		yourself
	
"*VIPVersion 22-6-97 | 7:34:56 pm 'ACV'*"! !

!CMDefinedPartImage class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartImage class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesImage!

metaPerspectivesImage
	^OrderedCollection new
		addLast: ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('ImageSelector' 'BorderWidth')));

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsImage!

metaSelectorsImage

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartImage selector: #metaSelectors target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 11)
		add: (METATerminalChildSpec new
			name: 'ImageSelector';
			basicSelector: #imageSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ImageSelector';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'BorderWidth';
			basicSelector: #borderWidth;
			type: #Number;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'BorderWidth';
			displaySelector: nil;
			yourself);
		yourself! !

!CMDefinedPartImage publicMethodsFor: 'accessing'!

borderWidth
	^borderWidth
"*VIPVersion 22-6-97 | 7:34:55 pm 'ACV'*"!

borderWidth: elValor

	| unValor unosStrings |
	unValor := (elValor isKindOf: Number)
		ifTrue: [ elValor]
		ifFalse: [ 
			(elValor isNil or: [ elValor isEmpty])
				ifTrue: [ false] 
				ifFalse: [ 	
					unosStrings := elValor  asArrayOfSubstrings.
					unosStrings isEmpty
						ifTrue: [  0]
						ifFalse: [ 
							unosStrings size > 1 
								ifTrue: [ Number readFrom: unosStrings first readStream]
								ifFalse: [ Number readFrom:  elValor]
						]
				]
		].
	
	unValor = borderWidth  ifTrue: [ ^self].

	borderWidth := unValor.
	self markDirty.
	self changed: #borderWidth!

imageSelector
	^imageSelector
"*VIPVersion 22-6-97 | 7:34:55 pm 'ACV'*"!

imageSelector: elValor

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
	
	unValor = imageSelector  ifTrue: [ ^self].

	imageSelector := unValor.
	self markDirty.
	self changed: #imageSelector! !

!CMDefinedPartImage publicMethodsFor: 'initialize-release'!

imageSelector: 			elSelectorImage
	layoutSymbol:		elLayoutSymbol
	layoutGroup:		elLayoutGroup
	borderWidth:		elBorderWidth
	
	imageSelector	:= elSelectorImage.
	layoutSymbol	:=	elLayoutSymbol.
	layoutGroup	:=	elLayoutGroup.
	borderWidth	:=	elBorderWidth.
	^self

"*VIPVersion 22-6-97 | 7:34:55 pm 'ACV'*"!

initialize
	super initialize.


	imageSelector	:= nil.
	borderWidth	:= nil!

release
	imageSelector	:= nil.
	layoutSymbol	:= nil.
	layoutGroup	:= nil.
	borderWidth	:= nil.
		super release

"*VIPVersion 22-6-97 | 7:34:55 pm 'ACV'*"! !

!CMDefinedPartImage publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder  withDefaultLayouts: theLayoutRectangles

	|  aLayout   aLookPreferences unVisualComponent aComponentSpec |

	theDefinedEditor isRemoteClient ifTrue: [ 
		^self remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder  withDefaultLayouts: theLayoutRectangles].
	theDefinedEditor isRemoteServer ifTrue: [ 
		^self remoteServerBuilderSpecFrom: theDefinedEditor with: theBuilder  withDefaultLayouts: theLayoutRectangles].

	unVisualComponent := 	PROPluggableGraphView on: theDefinedEditor 
		aspect: self imageSelector.
	theBuilder arbitraryComponentAt: self name asSymbol put: unVisualComponent.

	aLayout := theDefinedEditor  layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles.
	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.


	aComponentSpec := PROArbitraryComponentSpec new.
	aComponentSpec
		name:  self name;
		layout: aLayout; 
		component: self name asSymbol;
		colors: aLookPreferences;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) .


	^aComponentSpec
"*VIPVersion 12-7-97 | 1:37:12 am 'ACV'*"! !

!CMDefinedPartImage publicMethodsFor: 'persistence-code'!

initFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	self imageSelector: 		(theValues size < 13 ifTrue: [nil] ifFalse: [ theValues at: 13]). 
	self borderWidth: 			(theValues size < 14 ifTrue: [nil] ifFalse: [ theValues at: 14]).!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  imageSelector); nextPutAll: aSep;  nextPutAll: (self pcForV:  self  borderWidth); cr! !

!CMDefinedPartImage publicMethodsFor: 'save defined parts'!

innerSaveForVisualOnStream: elStream
	super innerSaveForVisualOnStream: elStream.
	elStream nextPutAll: 
		self imageSelector printString , '	',
		self layoutSymbol printString , '	',
		self layoutGroup printString , '	',
		self borderWidth printString , '	'

"*VIPVersion 22-6-97 | 7:34:56 pm 'ACV'*"! !

!CMDefinedPartList class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Lista'
"*VIPVersion 22-6-97 | 7:35:05 pm 'ACV'*"!

kind
	^#list
"*VIPVersion 22-6-97 | 7:35:05 pm 'ACV'*"! !

!CMDefinedPartList class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 22-6-97 | 7:35:05 pm 'ACV'*"! !

!CMDefinedPartList class publicMethodsFor: 'instance creation'!

named: elNombre
	printItems: elPrintItems
	oneItem: elOneItem
	aspect: elAspect
	change: elChange
	list: elList
	menu: elMenu
	initialSelection: elInitialSelection
	useIndex: elUseIndex
	layoutSymbol: elLayoutSymbol
	layoutGroup: elLayoutGroup 
	selectorVisible:		elSelectorVisible
	valorVisible:			elValorVisible
	selectorVisual:		elSelectorVisual
	valorVisual:			elValorVisual
	selectorActivo:		elSelectorActivo
	valorActivo:			elValorActivo

	^(super named: elNombre) 
		printItems: elPrintItems
		oneItem: elOneItem
		aspect: elAspect
		change: elChange
		list: elList 	menu: elMenu
		initialSelection: elInitialSelection
		useIndex: elUseIndex
		layoutSymbol: elLayoutSymbol
		layoutGroup: elLayoutGroup;
		selectorVisible:		elSelectorVisible;
		valorVisible:			elValorVisible;
		selectorVisual:		elSelectorVisual;
		valorVisual:			elValorVisual;
		selectorActivo:		elSelectorActivo;
		valorActivo:			elValorActivo;
		yourself

"*VIPVersion 22-6-97 | 7:35:05 pm 'ACV'*"!

printItems: elPrintItems oneItem: elOneItem aspect: elAspect change: elChange list: elList menu: elMenu initialSelection: elInitialSelection useIndex: elUseIndex layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup 

	^super new 
		printItems: elPrintItems 	oneItem: elOneItem 	aspect: elAspect 	change: elChange 	list: elList 	menu: elMenu 	initialSelection: elInitialSelection 	useIndex: elUseIndex 	layoutSymbol: elLayoutSymbol 	layoutGroup: elLayoutGroup
"*VIPVersion 22-6-97 | 7:35:05 pm 'ACV'*"! !

!CMDefinedPartList class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartList class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesList!

metaPerspectivesList
	^OrderedCollection new
		addLast: ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('PrintItems' 'OneItem'  'Aspect'  'Change'  'List'  'Menu'  'InitialSelection'  'UseIndex'  'UseScrollBar'  'TextStyle')));
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsList!

metaSelectorsList

	"METAChildSpecAutoViewEditor openOn: self selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 19)
		add: (METATerminalChildSpec new
			name: 'PrintItems';
			basicSelector: #printItems;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'PrintItems';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'OneItem';
			basicSelector: #oneItem;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'OneItem';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'Aspect';
			basicSelector: #aspect;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Aspect';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'Change';
			basicSelector: #change;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Change';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'List';
			basicSelector: #list;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'List';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'Menu';
			basicSelector: #menu;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Menu';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'InitialSelection';
			basicSelector: #initialSelection;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'InitialSelection';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'UseIndex';
			basicSelector: #useIndex;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'UseIndex';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'UseScrollBar';
			basicSelector: #useScrollBar;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'UseScrollBar';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'TextStyle';
			basicSelector: #textStyle;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TextStyle';
			displaySelector: nil;
			yourself);
		yourself! !

!CMDefinedPartList class publicMethodsFor: 'persistence-code'!

sizeSymbolAtValue: theValue
	theValue = 30 ifTrue: [ ^#minListHeight].
	theValue = 40 ifTrue: [ ^#midListHeight].
	theValue = 50 ifTrue: [ ^#bigFieldHeight].
	^nil!

sizeValueAtSymbol: theSymbol
	theSymbol = #minFieldHeight ifTrue: [ ^30].
	theSymbol = #midFieldHeight ifTrue: [ ^40].
	theSymbol = #bigFieldHeight ifTrue: [ ^50].
	^nil! !

!CMDefinedPartList publicMethodsFor: 'accessing'!

aspect
	^aspect
"*VIPVersion 22-6-97 | 7:35:01 pm 'ACV'*"!

aspect: elValor

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
	
	unValor = aspect  ifTrue: [ ^self].

	aspect := unValor.
	self markDirty.
	self changed: #aspect!

change
	^change
"*VIPVersion 22-6-97 | 7:35:01 pm 'ACV'*"!

change: elValor

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
	
	unValor = change  ifTrue: [ ^self].

	change := unValor.
	self markDirty.
	self changed: #change!

initialSelection
	^initialSelection
"*VIPVersion 22-6-97 | 7:35:02 pm 'ACV'*"!

initialSelection: elValor

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
	
	unValor = initialSelection  ifTrue: [ ^self].

	initialSelection := unValor.
	self markDirty.
	self changed: #initialSelection!

list
	^list
"*VIPVersion 22-6-97 | 7:35:02 pm 'ACV'*"!

list: elValor

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
	
	unValor = list  ifTrue: [ ^self].

	list := unValor.
	self markDirty.
	self changed: #list!

menu
	^menu
"*VIPVersion 22-6-97 | 7:35:02 pm 'ACV'*"!

menu: elValor

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
	
	unValor = menu  ifTrue: [ ^self].

	menu := unValor.
	self markDirty.
	self changed: #menu!

oneItem
	^oneItem
"*VIPVersion 22-6-97 | 7:35:03 pm 'ACV'*"!

oneItem: elValor

	| unValor unosStrings |
	unValor := elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
					(elValor isNil or: [ elValor isEmpty])
						ifTrue: [ false] 
						ifFalse: [ 	
							unosStrings := elValor  asArrayOfSubstrings.
							unosStrings isEmpty
								ifTrue: [ false]
								ifFalse: [ 
									unosStrings size > 1
										ifTrue: [ unosStrings first = true printString]
										ifFalse: [ elValor = true printString]
								]
						]
				]
		].
	
	unValor = oneItem  ifTrue: [ ^self].

	oneItem := unValor.
	self markDirty.
	self changed: #oneItem!

printItems
	^printItems
"*VIPVersion 22-6-97 | 7:35:03 pm 'ACV'*"!

printItems: elValor

	| unValor unosStrings |
	unValor := elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
					(elValor isNil or: [ elValor isEmpty])
						ifTrue: [ false] 
						ifFalse: [ 	
							unosStrings := elValor  asArrayOfSubstrings.
							unosStrings isEmpty
								ifTrue: [ false]
								ifFalse: [ 
									unosStrings size > 1
										ifTrue: [ unosStrings first = true printString]
										ifFalse: [ elValor = true printString]
								]
						]
				]
		].
	
	unValor = printItems  ifTrue: [ ^self].

	printItems := unValor.
	self markDirty.
	self changed: #printItems!

textStyle
	^textStyle
"*VIPVersion 22-6-97 | 7:35:03 pm 'ACV'*"!

textStyle: elValor

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
	
	unValor = textStyle  ifTrue: [ ^self].

	textStyle := unValor.
	self markDirty.
	self textStyle: #textStyle!

useIndex
	^useIndex
"*VIPVersion 22-6-97 | 7:35:03 pm 'ACV'*"!

useIndex: elValor

	| unValor unosStrings |
	unValor := elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
					(elValor isNil or: [ elValor isEmpty])
						ifTrue: [ false] 
						ifFalse: [ 	
							unosStrings := elValor  asArrayOfSubstrings.
							unosStrings isEmpty
								ifTrue: [ false]
								ifFalse: [ 
									unosStrings size > 1
										ifTrue: [ unosStrings first = true printString]
										ifFalse: [ elValor = true printString]
								]
						]
				]
		].
	
	unValor = useIndex  ifTrue: [ ^self].

	useIndex := unValor.
	self markDirty.
	self changed: #useIndex!

useScrollBar
	^useScrollBar

"*VIPVersion 22-6-97 | 7:35:04 pm 'ACV'*"!

useScrollBar: elValor

	| unValor unosStrings |
	unValor := elValor == true 
		ifTrue: [ true]
		ifFalse: [ 
			elValor == false
				ifTrue: [ false]
				ifFalse: [ 
					(elValor isNil or: [ elValor isEmpty])
						ifTrue: [ false] 
						ifFalse: [ 	
							unosStrings := elValor  asArrayOfSubstrings.
							unosStrings isEmpty
								ifTrue: [ false]
								ifFalse: [ 
									unosStrings size > 1
										ifTrue: [ unosStrings first = true printString]
										ifFalse: [ elValor = true printString]
								]
						]
				]
		].
	
	unValor = useScrollBar  ifTrue: [ ^self].

	useScrollBar := unValor.
	self markDirty.
	self changed: #useScrollBar! !

!CMDefinedPartList publicMethodsFor: 'ayuda'!

explicacion
	
	^'(Lista) ', self nombre,   super explicacion
"*VIPVersion 22-6-97 | 7:35:04 pm 'ACV'*"!

stringParaListaAyudas
	| unNombre |
	unNombre := self nombre.
	((unNombre indexOfSubCollection: 'Lista ' startingAt: 1) = 1 and: [
		 unNombre size > 'Lista ' size
	]) ifTrue: [ 
		^'Lista de ', (unNombre copyFrom: 'Lista ' size  + 1 to: unNombre size)
	].

	^'Lista de ', unNombre
"*VIPVersion 22-6-97 | 7:35:04 pm 'ACV'*"! !

!CMDefinedPartList publicMethodsFor: 'initialize-release'!

initialize
	super initialize.


	printItems	:= nil.
	oneItem	:= nil.
	aspect	:= nil.
	change	:= nil.
	list	:= nil.
	menu	:= nil.
	initialSelection	:= nil.
	useIndex	:= nil.
	useScrollBar	:= nil.
	textStyle	:= nil.!

printItems: elPrintItems oneItem: elOneItem aspect: elAspect change: elChange list: elList menu: elMenu initialSelection: elInitialSelection useIndex: elUseIndex layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup 
	printItems := elPrintItems.
	oneItem := elOneItem.
	aspect := elAspect.
	change := elChange.
	list := elList.
	menu := elMenu.
	initialSelection := elInitialSelection.
	useIndex := elUseIndex.
	layoutSymbol := elLayoutSymbol.
	layoutGroup := elLayoutGroup.
	useScrollBar	:= true.
	^self
"*VIPVersion 22-6-97 | 7:35:04 pm 'ACV'*"!

release
	printItems	:= nil.
	oneItem	:= nil.
	aspect	:= nil.
	change	:= nil.
	list	:= nil.
	menu	:= nil.
	initialSelection	:= nil.
	useIndex	:= nil.
	layoutSymbol	:= nil.
	layoutGroup	:= nil.
	useScrollBar	:= nil.
	textStyle	:= nil.
	super release
	
"*VIPVersion 22-6-97 | 7:35:04 pm 'ACV'*"! !

!CMDefinedPartList publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles
	| aModel  aLookPreferences aLayout aSequenceViewSpec |

	theDefinedEditor isRemoteClient ifTrue: [ 
		^self remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder].
	theDefinedEditor isRemoteServer ifTrue: [ 
		^self remoteServerBuilderSpecFrom: theDefinedEditor with: theBuilder].

	aModel := self adaptorForCMDefinedPartList: self get: self aspect put: self change.

	aLayout := theDefinedEditor  layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles.

	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.
	
	aSequenceViewSpec := PROSequenceViewSpec new 
		name: self name;
		layout: aLayout; 
		model: aModel;
		menu: nil;
		hasVerticalScrollBar: self useScrollBar;
		colors: aLookPreferences;
		definedPart: self;
		definedEditor: theDefinedEditor;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) .

	^aSequenceViewSpec!

remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	|  aLayout   aLookPreferences aRemoteEditor aModel aSequenceViewSpec |

	(theDefinedEditor isRemoteClient not or: [ self forceLocal]) ifTrue: [ 
		^self builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	aRemoteEditor := theDefinedEditor remoteEditorModel.
	aRemoteEditor isNil ifTrue: [ 
		^self builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	aModel := self remoteAdaptorForCMDefinedPartList: self get: self aspect put: self change.

	aLayout := theDefinedEditor  layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles.
	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.
	
	aSequenceViewSpec := PRORemoteSequenceViewSpec new 
		name: self name;
		layout: aLayout; 
		model: aModel;
		menu: nil;
		hasVerticalScrollBar: self useScrollBar;
		colors: aLookPreferences;
		definedPart: self;
		definedEditor: aRemoteEditor;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) .

	^aSequenceViewSpec! !

!CMDefinedPartList publicMethodsFor: 'persistence-code'!

initFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	self printItems: 				(theValues size < 13 ifTrue: [nil] ifFalse: [ theValues at: 13]). 
	self oneItem: 				(theValues size < 14 ifTrue: [nil] ifFalse: [ theValues at: 14]).
	self aspect: 					(theValues size < 15 ifTrue: [nil] ifFalse: [ theValues at: 15]). 
	self change: 					(theValues size < 16 ifTrue: [nil] ifFalse: [ theValues at: 16]).
	self list: 						(theValues size < 17 ifTrue: [nil] ifFalse: [ theValues at: 17]). 
	self menu: 					(theValues size < 18 ifTrue: [nil] ifFalse: [ theValues at: 18]).
	self initialSelection: 		(theValues size < 19 ifTrue: [nil] ifFalse: [ theValues at: 19]). 
	self useIndex: 				(theValues size < 20 ifTrue: [nil] ifFalse: [ theValues at: 20]).
	self useScrollBar: 			(theValues size < 21 ifTrue: [nil] ifFalse: [ theValues at: 21]). 
	self textStyle: 				(theValues size < 22 ifTrue: [nil] ifFalse: [ theValues at: 22]).!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream  
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  printItems); nextPutAll: aSep;  nextPutAll: (self pcForV:  self  oneItem); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  aspect); nextPutAll: aSep;  nextPutAll: (self pcForV:  self  change); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  list); nextPutAll: aSep;  nextPutAll: (self pcForV:  self  menu); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  initialSelection); nextPutAll: aSep;  nextPutAll: (self pcForV:  self  useIndex); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  useScrollBar); nextPutAll: aSep;  nextPutAll: (self pcForV:  self  textStyle); cr! !

!CMDefinedPartList publicMethodsFor: 'save defined parts'!

innerSaveForVisualOnStream: elStream
	super innerSaveForVisualOnStream: elStream.
	elStream nextPutAll: 
		self printItems printString , '	',
		self oneItem printString , '	',
		self aspect printString , '	',
		self change printString , '	',
		self list printString , '	',
		self menu printString , '	',
		self initialSelection printString , '	',
		self useIndex printString , '	',
		self layoutSymbol printString , '	',
		self layoutGroup printString , '	',
		self useScrollBar printString , '	',
		self textStyle printString , '	'

"*VIPVersion 22-6-97 | 7:35:04 pm 'ACV'*"! !

!CMDefinedPartLongField class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'TextoLargo'!

kind
	^#longField! !

!CMDefinedPartMultiList class publicMethodsFor: 'accessing'!

kind
	^#multiList
"*VIPVersion 22-6-97 | 7:35:07 pm 'ACV'*"! !

!CMDefinedPartMultiList class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 22-6-97 | 7:35:07 pm 'ACV'*"! !

!CMDefinedPartMultiList class publicMethodsFor: 'instance creation'!

named: elNombre printItems: elPrintItems oneItem: elOneItem aspect: elAspect change: elChange list: elList menu: elMenu initialSelection: elInitialSelection useIndex: elUseIndex layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup columnsLayoutGroup: elColumnsLayoutGroup 

	^(super named: elNombre) 
		printItems: elPrintItems 	oneItem: elOneItem 	aspect: elAspect 	change: elChange 	list: elList 	menu: elMenu 	initialSelection: elInitialSelection 	useIndex: elUseIndex 	layoutSymbol: elLayoutSymbol 	layoutGroup: elLayoutGroup columnsLayoutGroup: elColumnsLayoutGroup
"*VIPVersion 22-6-97 | 7:35:07 pm 'ACV'*"!

named: elNombre
	printItems: elPrintItems
	oneItem: elOneItem aspect: elAspect
	change: elChange list: elList
	menu: elMenu 
	initialSelection: elInitialSelection 
	useIndex: elUseIndex 
	layoutSymbol: elLayoutSymbol 
	layoutGroup: elLayoutGroup 
	columnsLayoutGroup: elColumnsLayoutGroup 
	selectorVisible:		elSelectorVisible
	valorVisible:			elValorVisible


	^(super named: elNombre) 
		printItems: elPrintItems 	oneItem: elOneItem 	aspect: elAspect 	change: elChange 	list: elList 	menu: elMenu 	initialSelection: elInitialSelection 	useIndex: elUseIndex 	layoutSymbol: elLayoutSymbol 	layoutGroup: elLayoutGroup columnsLayoutGroup: elColumnsLayoutGroup;
		selectorVisible:		elSelectorVisible;
		valorVisible:			elValorVisible;
		yourself
	
"*VIPVersion 22-6-97 | 7:35:07 pm 'ACV'*"!

named: 						elNombre
	printItems: 				elPrintItems
	oneItem: 				elOneItem 
	aspect: 					elAspect
	change: 					elChange 
	list: 						elList
	menu: 					elMenu 
	initialSelection: 		elInitialSelection 
	useIndex: 				elUseIndex 
	layoutSymbol: 			elLayoutSymbol 
	layoutGroup: 			elLayoutGroup 
	columnsLayoutGroup: elColumnsLayoutGroup 
	selectorVisible:			elSelectorVisible
	valorVisible:				elValorVisible
	selectorVisual:			elSelectorVisual
	valorVisual:				elValorVisual
	selectorActivo:			elSelectorActivo
	valorActivo:				elValorActivo


	^(super named: elNombre) 
		printItems: elPrintItems 
		oneItem: elOneItem
		aspect: elAspect
		change: elChange
		list: elList 	menu: elMenu
		initialSelection: elInitialSelection
		useIndex: elUseIndex
		layoutSymbol: elLayoutSymbol
		layoutGroup: elLayoutGroup
		columnsLayoutGroup: elColumnsLayoutGroup;
		selectorVisible:		elSelectorVisible;
		valorVisible:			elValorVisible;
		selectorVisual:		elSelectorVisual;
		valorVisual:			elValorVisual;
		selectorActivo:		elSelectorActivo;
		valorActivo:			elValorActivo;
		yourself
	
"*VIPVersion 22-6-97 | 7:35:07 pm 'ACV'*"!

printItems: elPrintItems oneItem: elOneItem aspect: elAspect change: elChange list: elList menu: elMenu initialSelection: elInitialSelection useIndex: elUseIndex layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup columnsLayoutGroup: elColumnsLayoutGroup 

	^super new 
		printItems: elPrintItems 	oneItem: elOneItem 	aspect: elAspect 	change: elChange 	list: elList 	menu: elMenu 	initialSelection: elInitialSelection 	useIndex: elUseIndex 	layoutSymbol: elLayoutSymbol 	layoutGroup: elLayoutGroup columnsLayoutGroup: elColumnsLayoutGroup
"*VIPVersion 22-6-97 | 7:35:07 pm 'ACV'*"! !

!CMDefinedPartMultiList class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesMultiList!

metaPerspectivesMultiList
	^OrderedCollection new
		addLast: ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'ColumnsLayout'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('ColumnsLayoutGroup')));

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsMultiList!

metaSelectorsMultiList

	"METAChildSpecAutoViewEditor openOn: CMDefinedPartMultiList selector: #metaSelectorsMultiList target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 2)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ColumnsLayoutGroup';
			basicSelector: #columnsLayoutGroup;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ColumnsLayoutGroup';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Columns';
			basicSelector: #columns;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Columns';
			displaySelector: #name;
			canShowInTree: true;
			componentsClassName: #CMDefinedPartColumn;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			yourself);
		yourself! !

!CMDefinedPartMultiList publicMethodsFor: 'accessing'!

columnsLayoutGroup
	^columnsLayoutGroup
"*VIPVersion 22-6-97 | 7:35:06 pm 'ACV'*"!

columnsLayoutGroup: elValor

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
	
	unValor = columnsLayoutGroup  ifTrue: [ ^self].

	columnsLayoutGroup := unValor.
	self markDirty.
	self changed: #columnsLayoutGroup!

columnsLayoutRectangles
	^columnsLayoutRectangles! !

!CMDefinedPartMultiList publicMethodsFor: 'ayuda'!

mostrarEnAyudaBrowser
	^((self name indexOfSubCollection: 'Cabecera' startingAt: 1) = 1) not
"*VIPVersion 22-6-97 | 7:35:06 pm 'ACV'*"! !

!CMDefinedPartMultiList publicMethodsFor: 'column initialize-release'!

columnsRelease
	"Generated by ISF/AD. Do not modify"
	self columns do: [:each | self columnsRemove: each]! !

!CMDefinedPartMultiList publicMethodsFor: 'columns accessing'!

columnNamed: theName
	"Generated by ISF/AD. Do not modify"
	theName isNil ifTrue: [ ^nil].
	^self columnsPrivate detect: [:anAT | anAT name = theName] ifNone: [nil]!

columns
	"Generated by ISF/AD. Do not modify"
	^self columnsPrivate copy!

columns: lasParts
	self columnsRelease.
	columns := lasParts.
	columns isNil ifFalse: [ 
		columns do: [:aPart | aPart containerPrivate: self]]!

columnsAsArray
	"Generated by ISF/AD. Do not modify"
	^self columns asArray!

columnWithLayoutSymbol: theLayoutSymbol
	theLayoutSymbol isNil ifTrue: [ ^nil].

	^self columns detect: [:aPart | theLayoutSymbol = aPart layoutSymbol] ifNone: [ nil]! !

!CMDefinedPartMultiList publicMethodsFor: 'columns creation'!

columnsLinkCreate
	
	| aPart |

	aPart := CMDefinedPartColumn named: 'newColumn'.
	self columnsAdd: aPart.
	self registerNewLayoutRectangle: aPart layoutRectangle.	
	self changed: #columns.
	^aPart! !

!CMDefinedPartMultiList publicMethodsFor: 'columns modifying'!

columnsAdd: aValue
	"Generated by ISF/AD. Do not modify"
	(self columnsIncludes: aValue) ifTrue: [^self columnsMoveBottom: aValue].
	(self columnsPrivateAdd: aValue) containerPrivate: self.
	self changed: #columns.
	^aValue!

columnsMoveBottom: aValue
	"Generated by ISF/AD. Do not modify"
	| aRes |
	(self columnsIncludes: aValue) ifFalse: [^aValue].
	(self columns indexOf: aValue) = self columnsSize ifTrue: [^aValue].
	self columnsPrivate remove: aValue.
	aRes := self columnsPrivateAdd: aValue.
	self reorderLayoutRectangles.
	self changed: #columns.
	^aRes!

columnsMoveDown: aValue
	"Generated by ISF/AD. Do not modify"
	| index aRes |
	(self columnsIncludes: aValue) ifFalse: [^aValue].
	(index := self columns indexOf: aValue) = self columnsSize ifTrue: [^aValue].
	index = (self columnsSize -1)
		ifTrue:
			[self columnsPrivate remove: aValue.
			aRes := self columnsPrivateAdd: aValue.
			self reorderLayoutRectangles.
			^aRes].
	aRes := self columnsPrivateMove: aValue beforeIndex: index + 2.
	self reorderLayoutRectangles.
	self changed: #columns.
	^aRes!

columnsMoveTop: aValue
	"Generated by ISF/AD. Do not modify"
	(self columnsIncludes: aValue) ifFalse: [^aValue].
	(self columns indexOf: aValue) = 1 ifTrue: [^aValue].
	self columnsPrivateMove: aValue beforeIndex: 1.
	self reorderLayoutRectangles.
	self changed: #columns.!

columnsMoveUp: aValue
	"Generated by ISF/AD. Do not modify"
	| index aRes |
	(self columnsIncludes: aValue) ifFalse: [^aValue].
	(index := self columns indexOf: aValue) = 1 ifTrue: [^aValue].
	aRes := self columnsPrivateMove: aValue beforeIndex: index - 1.

	self reorderLayoutRectangles.
	self changed: #columns.

	^aRes!

columnsRemove: aValue
	"Generated by ISF/AD. Do not modify"
	(self columnsPrivate remove: aValue ifAbsent: [^aValue]) containerPrivate: nil.
	self reorderLayoutRectangles.

	self changed: #columns.
	^aValue! !

!CMDefinedPartMultiList publicMethodsFor: 'columns private'!

columnsPrivate
	"Generated by ISF/AD. Do not modify"
	columns isNil
		ifTrue: [self initColumns].
	^columns!

columnsPrivate: lasParts
	columns := lasParts.!

columnsPrivateAdd: aValue
	"Generated by ISF/AD. Do not modify"
	self columnsPrivate add: aValue.
	self changed: #columns.
	self markDirty.

	^aValue!

columnsPrivateMove: aValue beforeIndex: anIndex
	"Generated by ISF/AD. Do not modify"
	| obj |
	obj := self columns at: anIndex.
	self columnsPrivate remove: aValue.
	self columnsPrivate add: aValue before: obj.
	self markDirty.

	self changed: #columns.
	^aValue!

columnsPrivateRemove: aValue
	"Generated by ISF/AD. Do not modify"
	self columnsPrivate remove: aValue.
	self markDirty.

	self changed: #columns.
	^aValue! !

!CMDefinedPartMultiList publicMethodsFor: 'columns testing'!

columnsIncludes: aValue
	"Generated by ISF/AD. Do not modify"
	^columns isNil
		ifTrue: [false]
		ifFalse: [self columns includes: aValue]!

columnsSize
	"Generated by ISF/AD. Do not modify"
	^columns isNil
		ifTrue: [0]
		 ifFalse: [columns size]! !

!CMDefinedPartMultiList publicMethodsFor: 'initialize-release'!

initialize
	super initialize.


	columnsLayoutGroup := nil.
	columnsLayoutRectangles := Array new.
	columns := OrderedCollection new!

printItems: elPrintItems oneItem: elOneItem aspect: elAspect change: elChange list: elList menu: elMenu initialSelection: elInitialSelection useIndex: elUseIndex layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup  columnsLayoutGroup: elColumnsLayoutGroup

	printItems := elPrintItems.
	oneItem := elOneItem.
	aspect := elAspect.
	change := elChange.
	list := elList.
	menu := elMenu.
	initialSelection := elInitialSelection.
	useIndex := elUseIndex.
	layoutSymbol := elLayoutSymbol.
	layoutGroup := elLayoutGroup.
	columnsLayoutGroup := elColumnsLayoutGroup.
	useScrollBar	:= true.

	^self
"*VIPVersion 22-6-97 | 7:35:06 pm 'ACV'*"!

release
	columnsLayoutGroup := nil.
	columnsLayoutRectangles := nil.
	self columnsRelease.
	columns := OrderedCollection new.
		
	super release! !

!CMDefinedPartMultiList publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles
	|  aLayout aSequenceViewSpec aModel     aColumnsLayout aLookPreferences |

	theDefinedEditor isRemoteClient ifTrue: [ 
		^self remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].
	theDefinedEditor isRemoteServer ifTrue: [ 
		^self remoteServerBuilderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	aModel := "PROSelectionInList definedPart: self definedEditor:" theDefinedEditor adaptorForCMDefinedPartMultiList: self.

	aLayout := theDefinedEditor  layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles.

	aColumnsLayout := theDefinedEditor layoutsIn: self columnsLayoutGroup withDefaultLayouts: self columnsLayoutRectangles.
	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.

	aSequenceViewSpec := PROSequenceMultiViewSpec new 
		name: self name;
		layout: aLayout; 
		model: aModel;
		menu: nil;
		tabable: (self useScrollBar and: [ self oneItem not]);
		hasVerticalScrollBar: (self useScrollBar and: [ self oneItem not]);
		columnsLayout: aColumnsLayout;
		hasBorder: (self oneItem not);
		colors: aLookPreferences;
		definedPart: self;
		definedEditor: theDefinedEditor;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) .

	^aSequenceViewSpec
"*VIPVersion 16-7-97 | 3:27:22 am 'ACV'*"!

remoteClientBuilderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	|  aLayout   aLookPreferences aRemoteEditor aModel aSequenceViewSpec aColumnsLayout |

	(theDefinedEditor isRemoteClient not or: [ self forceLocal]) ifTrue: [ 
		^self builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	aRemoteEditor := theDefinedEditor remoteEditorModel.
	aRemoteEditor isNil ifTrue: [ 
		^self builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles].

	aModel :=  theDefinedEditor remoteAdaptor: aRemoteEditor forCMDefinedPartMultiList: self.

	aLayout := theDefinedEditor  layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles.
	aColumnsLayout := theDefinedEditor layoutsIn: self columnsLayoutGroup withDefaultLayouts: self columnsLayoutRectangles.
	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.

	aSequenceViewSpec := PRORemoteSequenceMultiViewSpec new 
		name: self name;
		layout: aLayout; 
		model: aModel;
		menu: nil;
		tabable: (self useScrollBar and: [ self oneItem not]);
		hasVerticalScrollBar: (self useScrollBar and: [ self oneItem not]);
		columnsLayout: aColumnsLayout;
		hasBorder: (self oneItem not);
		colors: aLookPreferences;
		definedPart: self;
		definedEditor: aRemoteEditor;
		initiallyInvisible:  (self selectorVisible isNil not and: [ (self selectorVisible = true) not]) ;
		initiallyDisabled:  (self selectorActivo isNil not and: [ (self selectorActivo = true) not]) .

	^aSequenceViewSpec! !

!CMDefinedPartMultiList publicMethodsFor: 'layout'!

initializeLayoutsFromEditor: theEditor
	| someLayouts |

	theEditor isNil  ifTrue: [ ^self].

	someLayouts := theEditor defaultLayoutsFor: self columnsLayoutGroup.

	(someLayouts isNil or: [someLayouts isEmpty]) ifTrue: [ ^self].

	columnsLayoutRectangles := someLayouts.!

registerNewLayoutRectangle: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [ ^self].

	columnsLayoutRectangles := columnsLayoutRectangles , (Array with: theLayoutRectangle)!

reorderLayoutRectangles
	| someParts someLayoutRectangles  |
	someParts := self columns.
	(someParts isNil or: [ someParts isEmpty]) ifTrue: [ ^self].

	someLayoutRectangles := OrderedCollection new: self columnsSize.
	self columnsPrivate do: [:aPart | | aLayoutRectangle |
		aPart hasLayoutRectangle ifTrue: [ 
			aLayoutRectangle := aPart layoutRectangle.
			aLayoutRectangle isNil ifFalse: [ someLayoutRectangles add: aLayoutRectangle]]].

	columnsLayoutRectangles := someLayoutRectangles! !

!CMDefinedPartMultiList publicMethodsFor: 'persistence-code'!

buildOneColumnLayoutRectangleFromValues: theValues

	| aLayoutRectangle |
	theValues isNil ifTrue: [ ^nil].

	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].
	
	aLayoutRectangle := Array
		with: (theValues size < 1 ifTrue: [nil] ifFalse: [ theValues at: 1])
		with: (theValues size < 2 ifTrue: [nil] ifFalse: [ theValues at: 2])
		with: (theValues size < 3 ifTrue: [nil] ifFalse: [ (theValues at: 3) first @ ((theValues at: 3) at: 2) ])
		with: (theValues size < 4 ifTrue: [nil] ifFalse: [ (theValues at: 4) first @ ((theValues at: 4) at: 2) ])
		with: (theValues size < 5 ifTrue: [nil] ifFalse: [ Array with: (theValues at: 5) first with: ((theValues at: 5) at: 2) ])
		with: (theValues size < 6 ifTrue: [nil] ifFalse: [ theValues at: 6]).
	
	^aLayoutRectangle!

columnLayoutsPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS someLayoutRectangles |

	theStream isNil ifTrue:  [^nil].

	someLayoutRectangles := self columnsLayoutRectangles.
	someLayoutRectangles isNil ifTrue: [ 
		theStream nextPutAll: nil printString.
		^self
	].

	anIS := theIS , self indentStringForPersistenceAsCode.

	theStream nextPutAll: anIS; nextPutAll: '( ';  nextPutAll: (self pcForV: self class layoutsBlockPersistenceSymbol); cr.
	someLayoutRectangles do: [:aLayoutRectangle |
		aLayoutRectangle isNil ifFalse: [ 
			self oneColumnLayout: aLayoutRectangle persistenceAsCodeStringOn: theStream indent: anIS]
	].
	theStream  nextPutAll: anIS; nextPutAll: ')'; cr!

initColumnsLayoutRectanglesFromValues: theValues
	
	| someLayoutRectangles |

	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].
		
	someLayoutRectangles := OrderedCollection new: theValues size.
	2 to: theValues size do: [:anIndex | | someValues  aNewLayoutRectangle |
		someValues := theValues at: anIndex.
		someValues isNil ifFalse: [ 
			aNewLayoutRectangle := self buildOneColumnLayoutRectangleFromValues: someValues.
			aNewLayoutRectangle isNil ifFalse: [  someLayoutRectangles add: aNewLayoutRectangle]]
	].

	columnsLayoutRectangles := someLayoutRectangles asArray!

initFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	self columnsLayoutGroup: 				(theValues size < 23 ifTrue: [nil] ifFalse: [ theValues at: 23]).

	theValues size < 24 ifFalse: [ self initColumnsLayoutRectanglesFromValues: (theValues at: 24)].

	self initPartColumnsFromColumnsLayoutRectangles!

initPartColumnsFromColumnsLayoutRectangles
	
	(columnsLayoutRectangles isNil or: [ columnsLayoutRectangles isEmpty]) ifTrue: [ ^self].
		
	columnsLayoutRectangles  do: [:aLayoutRectangle | | aNewColumn |
		aLayoutRectangle isNil ifFalse: [ 
			aNewColumn := CMDefinedPartColumn newFromLayoutRectangle: aLayoutRectangle.
			self columnsAdd: aNewColumn
		]
	].!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  columnsLayoutGroup); cr.

	self columnLayoutsPersistenceAsCodeStringOn: theStream indent: anIS!

oneColumnLayout: theLayoutRectangle persistenceAsCodeStringOn: theStream indent: theIS

	| anIS |

	theStream isNil ifTrue:  [^nil].

	anIS := theIS , self indentStringForPersistenceAsCode.

	theStream nextPutAll: anIS; nextPutAll: '( ';  nextPutAll: (self pcForV: (self class layoutNameFrom: theLayoutRectangle));  space;
		nextPutAll:  (self pcForV: (self class relativeHeightFrom: theLayoutRectangle)); space;
			nextPutAll: '('; nextPutAll: (self pcForV: (self class minExtentXFrom: theLayoutRectangle)); space; 
		nextPutAll: (self pcForV: (self class minExtentYFrom: theLayoutRectangle)); space; nextPutAll: ' ) ';
			nextPutAll: '('; nextPutAll: (self pcForV: (self class  maxExtentXFrom: theLayoutRectangle)); space;
		nextPutAll: (self pcForV: (self class  maxExtentYFrom: theLayoutRectangle)); space; nextPutAll: ' ) ';
		nextPutAll: '( '; nextPutAll: (self pcForV: (self class  relXStartFrom: theLayoutRectangle)); space; 
			nextPutAll: (self pcForV: (self class  relXWidthFrom: theLayoutRectangle)); space; nextPutAll: ' ) ';
		nextPutAll: ((self class  mustAdvanceRowFrom: theLayoutRectangle) == true) printString; space;
		nextPutAll: ')'; cr.! !

!CMDefinedPartMultiList publicMethodsFor: 'remote'!

sampleRemoteValue
	^Array with: (Array with: 'One C1 Remote' with: 'One C2 Remote') with: (Array with: 'Other C1 Remote' with: 'Other C2 Remote')! !

!CMDefinedPartMultiList publicMethodsFor: 'save defined parts'!

innerSaveForVisualOnStream: elStream
	super innerSaveForVisualOnStream: elStream.
	elStream nextPutAll: 
		self columnsLayoutGroup printString , '	'

"*VIPVersion 22-6-97 | 7:35:06 pm 'ACV'*"! !

!CMDefinedPartMultiList publicMethodsFor: 'subPart initialize-release'!

initColumns
	"Generated by ISF/AD. Do not modify"
	columns := OrderedCollection new.! !

!CMDefinedPartMultiList publicMethodsFor: 'tests'!

isMultiList
	^true! !

!CMDefinedPartSep class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Sep'!

kind
	^#sep! !

!CMDefinedPartSep class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartSep class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesSep!

metaPerspectivesSep
	^OrderedCollection new

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsSep!

metaSelectorsSep
	
	"METAChildSpecAutoViewEditor openOn: CMDefinedPartSep selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.
	^(OrderedCollection new: 9)
		yourself! !

!CMDefinedPartSep publicMethodsFor: 'accessing'!

layoutSymbol: theValue

	(theValue =  self kind) ifFalse: [ ^self].
	super layoutSymbol: theValue!

name: elNombre
	elNombre = self kind ifFalse: [ ^self].
	super name: elNombre!

nombre: elNombre
	elNombre = self kind ifFalse: [ ^self].
	super nombre: elNombre! !

!CMDefinedPartSep publicMethodsFor: 'accessing-layout'!

layoutName: theValue
	
	(theValue =  self kind) ifFalse: [ ^self].

	super layoutName: theValue! !

!CMDefinedPartSep publicMethodsFor: 'initialize-release'!

initialize
	super initialize.

	name	:= self kind.
	layoutSymbol := self kind.

	self relativeHeight: 0.002.
	self minExtentX: 0.
	self minExtentY: 4.
	self maxExtentX: 0.
	self maxExtentY: 0.
	self relXStart: 0.
	self relXWidth: 1.
	self mustAdvanceRow: true.! !

!CMDefinedPartSep publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles
	^nil! !

!CMDefinedPartSep publicMethodsFor: 'layout'!

initLayoutRectangleFromIdentical: theLCA

	super initLayoutRectangleFromIdentical: theLCA! !

!CMDefinedPartSep publicMethodsFor: 'persistence-code'!

layoutPersistenceAsCodeStringOn: theStream indent: theIS

	^super layoutPersistenceAsCodeStringOn: theStream indent: theIS! !

!CMDefinedPartsInstaller class publicMethodsFor: 'cmDefinedParts persistence'!

exampleWithMetaInfoDefinedWindowStore

	"(CMDefinedPart newFromPersistenceAsCode: CMDefinedPartsInstaller exampleWithMetaInfoDefinedWindowStore) browse"

	self ojoCMDefinedParts.

	^   #( window 'Ejemplo CMDefinedPanelAppModel'
	nil
	nil
	nil
	nil
	nil
'Ejemplo CMDefinedPanleAppModel'
	exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
	32 32 150 100 640 480 1400 1000
	CMDefinedPartsInstaller exampleWithMetaInfoDefinedWindowStore
	allAllowed
	'Clinica'
	KRSimpleTranslationHolder
	kronoSimpleUITranslationStore
	( refToModel 'KronoSimple' kronoSimpleStore KRSimpleMetaInfoHolder )

	  (allowedRootTypes
		 ( refToType 'Clinica' 'Nucleo'  ) 
	   )
	  nil
	(parts
	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title1LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		cif exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		'C.I.F.'
		yourself left
		0
	   )

	  ( field 'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field1LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'cif'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title2LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		razonSocial exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		nil
		yourself left
		0
	   )

	  ( field 'Field 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field2LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'razonSocial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	  ( sep #sep
		nil
		nil
		nil
		nil
		nil
		cooperative sep nil
		( layoutRectangle 0.1 (0 12 ) (0 30 ) ( 0 1 ) true )
nil
	   )

	  ( title 'Title 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative title3LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.05 0.3 ) false )
nil
		nombreComercial exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup
		nil
		yourself left
		0
	   )

	  ( field 'Field 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		nil
		nil
		true
		nil
		true
		cooperative field3LayoutSymbol exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup
		( layoutRectangle 0.1 (0 30 ) (0 50 ) ( 0.4 0.55 ) true )
nil
		nil nil
		nil nil
		yourself left
		0
		nil
		 ( refToAttribute 'nombreComercial'  ( refToType 'PersonaJuridica' 'TiposGenerales'  )  ) 

	   )

	 )

   )! !

!CMDefinedPartsInstaller class publicMethodsFor: 'examples'!

example
"

CMDefinedPartsInstaller 
persistCMDefinedWindow: 
(PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor)  filter: #dirty.


(PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor)  halt persistenceAsCodeStringChunksFilter: true doSubChunks: true.


rotowareVWLanzador open


Notarias
Smalltalk at: #ClientNotarias put: Notarias
METAPathFinderScopedApplicationBrowser openForObject: InterModelUpdater interModelUpdater models last

METAScopedApplicationBrowser openForObject: InterModelUpdater interModelUpdater models last
ObjectMemory verboseGrowMemoryBy: 32000000


PROCachedWindowDefinitionsHolder clearCMDefinedWindowCache.

METAScopedApplicationBrowser openForObject: (PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor)


(PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor) persistenceAsCodeStringChunks



(PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor) cleanDirtyMark persistenceAsCodeStringChunksFilter: #dirty doSubChunks: true

(PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor)  halt persistenceAsCodeStringChunksFilter: true doSubChunks: true

PROCachedWindowDefinitionsHolder clearCMDefinedWindowCache.

CMDefinedPartsInstaller 
persistCMDefinedWindow: 
(PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor)  filter: #dirty





 METAScopedApplicationBrowser openForObject:  self.
METAScopedApplicationBrowser openForObject: 
	(CMDefinedPart newFromPersistenceAsCode: 
		(Compiler evaluate: self persistenceAsCodeString))

(PROCachedWindowDefinitionsHolder definedWindowFor: #ClinicaHistoriaEditor) testIfAllSubPartsHaveSameLayoutGroups
 METAScopedApplicationBrowser openForObject: self

"!

example01WOMetaInfo

	"CMDefinedPartsInstaller example01WOMetaInfo browse"

	| someParts aDefinedWindow aTitlePart2 aFieldPart1 aFieldPart2 aFieldPart3 aTitlePart1 aTitlePart3 aSep1 aSep2 aSep3 |

	someParts := OrderedCollection new.
	
	aSep1 := CMDefinedPartSep named: 'sep1'.
	aSep1	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep1.

	aTitlePart1 := CMDefinedPartTitle
		named: 							'Title 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#cif 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart1.


	aFieldPart1 := CMDefinedPartField
		named: 							'Field 1 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field1LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field1LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							#field1Aspect
		change: 							#field1Change
		metaInfo:							nil
		valueExpression:				nil
		menu: 							#field1Menu
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart1.

	aSep2 := CMDefinedPartSep named: 'sep2'.
	aSep2	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep2.

	aTitlePart2 := CMDefinedPartTitle
		named: 							'Title 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#razonSocial 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart2.


	aFieldPart2 := CMDefinedPartField
		named: 							'Field 2 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field2LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field2LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							#field2Aspect
		change: 							#field2Change
		metaInfo:							nil
		valueExpression:				nil
		menu: 							#field2Menu
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart2.


	aSep3 := CMDefinedPartSep named: 'sep3'.
	aSep3	 layoutRectangle:	(Array with: #sep with: 1/10 with: 0@12 with: 0@30 with: (Array with: 0 with: 1) with: true).
	someParts add: aSep3.

	aTitlePart3 := CMDefinedPartTitle
		named: 							'Title 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#title3LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartTitleLayoutGroup 
		layoutRectangle:				(Array with: #title3LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.05 with: 0.3) with: false)
		layoutFrame: 					nil
		nlsSymbol: 						#nombreComercial 
		nlsGroup: 						#exampleCMDefinedPanelAppModelDefinedPartTitleNlsGroup 
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aTitlePart3.

	aFieldPart3 := CMDefinedPartField
		named: 							'Field 3 ExampleCMDefinedPanelAppModelDefinedWindowName'
		visibilityPredicate:				nil
		initiallyVisiblePredicate:  		true
		enablementPredicate: 			nil 
		initiallyEnabledPredicate:		true
		layoutMode:						CMDefinedPart layoutModeCooperativeSymbol
		layoutSymbol:					#field3LayoutSymbol 
		layoutGroup:						#exampleCMDefinedPanelAppModelDefinedPartFieldLayoutGroup 
		layoutRectangle:				(Array with: #field3LayoutSymbol 
												with: 1/10 with: 0@30 with: 0@50 with: (Array with: 0.4 with: 0.55) with: true)
		layoutFrame: 					nil
		aspect: 							#field3Aspect
		change: 							#field3Change
		metaInfo:							nil
		valueExpression:				nil
		menu: 							#field3Menu
		initialSelection: 					nil
		enhance: 						#yourself 
		justification:						#left
		borderWidth:					0.
	someParts add: aFieldPart3.

	aDefinedWindow := CMDefinedWindow named: 'ExampleWOMetaInfo'
		titleDefaultTranslation: 'Example Window WO MetaInfo'
		titleNlsSymbol: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsSymbol 
		titleNlsGroup: #exampleCMDefinedPanelAppModelDefinedWindowTitleNlsGroup
		minimumSizeLayoutGroup: #exampleCMDefinedPanelAppModelDefinedWindowMinimumSizeLayoutGroup
		storeClassName: self name asString
		storeMethodSelector: 'exampleWOMetaInfoDefinedWindowStore'
		parts: someParts.

	^aDefinedWindow! !

!CMDefinedPartsInstaller class publicMethodsFor: 'persistence-code'!

definedPartsProtocol
	^'cmDefinedParts persistence' copy!

existsGroupMethod: theSelector inClass: theClassName
	^(self groupMethod: theSelector inClass: theClassName) isNil not!

groupMethod: theSelector inClass: theClassName
	| aClassName aClass aSelector aCompiledMethod aMetaClass |
	(theSelector isNil or: [ theSelector isEmpty]) ifTrue: [ ^nil].
	(theClassName isNil or: [ theClassName isEmpty]) isNil ifTrue: [ ^nil].

	aClassName := theClassName asSymbol.
	aClass := Smalltalk at:  aClassName ifAbsent: [ nil].
	aClass isNil ifTrue: [ ^nil].

	aMetaClass := aClass class.

	aSelector := theSelector asSymbol.
	aCompiledMethod := aMetaClass compiledMethodAt: aSelector ifAbsent: [ nil].
	aCompiledMethod isNil ifTrue: [ ^nil].

	^Array with: aClass with: aCompiledMethod!

intoStream: theStream sourceHeaderForGroupMethod: theSelector inClass: theClassName

	theStream isNil ifTrue: [ ^self].

	theStream nextPutAll: theSelector; cr; cr.
	theStream nextPutAll: '	"(CMDefinedPart newFromPersistenceAsCode: ';
		nextPutAll: theClassName; nextPutAll: ' '; nextPutAll: theSelector; nextPutAll: ') browse"'; cr; cr.
	theStream nextPutAll: '	self ojoCMDefinedParts.'; cr; cr.
	theStream nextPutAll: '	^ '.!

isInteractive
	^false!

makeBackup
	^false!

ojoCMDefinedParts!

persistChunk: theChunk

	| aCMDefinedPart aSource aSelector aClassName aClass aMakeBackup aTargetSelector |

	theChunk isNil ifTrue: [ ^nil].

	aCMDefinedPart := theChunk first.
	aCMDefinedPart isNil ifTrue: [ ^nil].

	aSource := theChunk at: 2.
	(aSource isNil or: [aSource isEmpty]) ifTrue: [ ^nil].
	
	aSelector := aCMDefinedPart storeMethodSelector.
	(aSelector isNil or: [ aSelector isEmpty]) ifTrue: [ ^nil].
	aSelector := aSelector asSymbol.

	aClassName := aCMDefinedPart storeClassName.
	(aClassName isNil or: [ aClassName isEmpty]) isNil ifTrue: [ ^nil].
	aClassName := aClassName asSymbol.

	aClass := Smalltalk at:  aClassName ifAbsent: [ nil].
	aClass isNil ifTrue: [ ^nil].

	self isInteractive ifTrue: [
		(Dialog confirm: 'Do you really want to install on Class\' withCRs,
			aClassName ,'>>', aSelector,  ' ?' initialAnswer: false) ifFalse: [ ^nil]].

	Transcript show: 'Installing on Class ' , aClassName ,'>>', aSelector, '  ... '; cr.


	aMakeBackup := false.

	(aClass class includesSelector: aSelector) ifTrue: [
		 self isInteractive
			ifFalse: [	
				aMakeBackup := self makeBackup.
				Transcript show: '     ', aClassName , ' class already includes selector ' , aSelector; cr.
				aMakeBackup 
					ifTrue: [ Transcript show: '        Keeping backup copy as x', aSelector; cr]
					ifFalse: [ Transcript show: '        No Backup kept of' , aClassName ,'>>', aSelector; cr]
			]
			ifTrue: [ 
				aMakeBackup := Dialog 
					choose: aClassName , ' class already includes selector\' withCRs,
						aSelector, '\' withCRs,
						'Do you want to keep a backup copy of it as ', 'x', aSelector, ' ?'
					labels: #('Backup copy' 'No Backup' 'Cancel')
					values: #(true false nil)
					default: false.
				aMakeBackup isNil ifTrue: [ ^nil]]].

	aMakeBackup ifTrue: [ 
		self errorSignal handle: [:anException |  | aMsg |
			aMsg := '    Error making backup copy on Class ' , aClassName ,'>>x', 
				aSelector, '\' withCRs, 'Installation aborted'.
			self isInteractive ifTrue: [ Dialog warn: aMsg].
			Transcript show: aMsg;cr.
			^nil]
		do: [     | aMsg |
			aTargetSelector := aClass class 
				compile: 'x', (aClass class sourceCodeAt: aSelector)
				classified: self definedPartsProtocol
				notifying: nil.
			aTargetSelector isNil ifTrue: [ 
				aMsg := '    Backup copy in Class ' , aClassName ,'>>x',  aSelector, ' failed'.
				 self isInteractive ifTrue: [ Dialog warn: aMsg].
				Transcript show: aMsg;cr.
				^nil]]].


	self errorSignal handle: [:anException | | aMsg | 
		aMsg :=  '    Error installing on Class ' , aClassName ,'>>', aSelector, 
				'\Installation aborted' withCRs.
		self isInteractive ifTrue: [ Dialog warn: aMsg].
		Transcript show: aMsg; cr.
		^nil]
		do: [  | aSourceCodeStream aMsg |

			aSourceCodeStream := WriteStream on: (String new: 1024).
			self intoStream: aSourceCodeStream sourceHeaderForGroupMethod: aSelector 
				inClass: aClassName.

			aSourceCodeStream nextPutAll: aSource.

			aTargetSelector := aClass class 
				compile: aSourceCodeStream contents
				classified: self definedPartsProtocol
				notifying: nil.
			aTargetSelector isNil ifTrue: [ 
				aMsg := '    Installation on Class ' , aClassName ,'>>', aSelector, ' failed.'.
				self isInteractive ifTrue: [ Dialog warn: aMsg].
				Transcript show: aMsg; cr.
				^nil
			].
			Transcript show: '    Installation on Class ' , aClassName ,'>>', aSelector, ' OK'; cr.

	^self
]!

persistCMDefinedWindow: theCMDefinedWindow filter: theFilter

	| someResults someChunks |
	theCMDefinedWindow isNil ifTrue: [ ^nil].
	theCMDefinedWindow isWindow ifFalse: [ ^nil].

	someChunks := theCMDefinedWindow persistenceAsCodeStringChunksFilter: theFilter doSubChunks: true.
	someChunks isNil ifTrue: [ ^nil].

	someResults := someChunks collect: [:aChunk | self persistChunk: aChunk].
	^someResults!

sameSource: theSource groupMethod: theSelector inClass: theClassName

	| aClassAndMethod aSource aClass aMetaClass aStream aFullThisSource aRes |


	aClassAndMethod := self groupMethod: theSelector inClass: theClassName.
	((theSource isNil or: [ theSource isEmpty]) and: [  aClassAndMethod isNil]) ifTrue:  [ ^true].

	aClass := aClassAndMethod first.
	aMetaClass := aClass class.

	aSource := aMetaClass sourceCodeAt: theSelector ifAbsent: [ nil].
	((theSource isNil or: [ theSource isEmpty]) and: [  aSource isNil]) ifTrue:  [ ^true].
	aSource isNil ifTrue: [ ^false].

	aStream := WriteStream on: (String new: 1024 + aSource size).
	self intoStream: aStream sourceHeaderForGroupMethod: theSelector inClass: theClassName.
	aStream nextPutAll: theSource.

	aFullThisSource := aStream contents.

	aRes := aFullThisSource asArrayOfSubstrings = aSource asArrayOfSubstrings.

	^aRes!

sourceHeaderForGroupMethod: theSelector inClass: theClassName

	| aStream |
	aStream := WriteStream on: (String new: 1024).
	self intoStream: aStream sourceHeaderForGroupMethod: theSelector inClass: theClassName.
	^aStream contents! !

!CMDefinedPartSlider class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Deslizante'
"*VIPVersion 22-6-97 | 7:34:53 pm 'ACV'*"!

kind
	^#slider
"*VIPVersion 22-6-97 | 7:34:53 pm 'ACV'*"! !

!CMDefinedPartSlider class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 22-6-97 | 7:34:53 pm 'ACV'*"! !

!CMDefinedPartSlider class publicMethodsFor: 'instance creation'!

named: elNombre aspect: elAspect change: elChange  estilo: elEstilo
	layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup 

	^(super named: elNombre) 
		aspect: elAspect 	change: elChange    estilo: elEstilo
		layoutSymbol: elLayoutSymbol 	layoutGroup: elLayoutGroup
"*VIPVersion 22-6-97 | 7:34:53 pm 'ACV'*"!

named: 				elNombre
	aspect: 			elAspect
	change: 			elChange
	estilo: 				elEstilo	
	layoutSymbol: 		elLayoutSymbol
	layoutGroup: 		elLayoutGroup 
	selectorVisible:	elSelectorVisible
	valorVisible:		elValorVisible
	selectorVisual:		elSelectorVisual
	valorVisual:		elValorVisual
	selectorActivo:		elSelectorActivo
	valorActivo:		elValorActivo

	^(super named: elNombre) 
		aspect: 			elAspect
		change: 			elChange
		estilo: 			elEstilo	
		layoutSymbol: 	elLayoutSymbol
		layoutGroup: 	elLayoutGroup;
		selectorVisible:	elSelectorVisible;
		valorVisible:		elValorVisible;
		selectorVisual:	elSelectorVisual;
		valorVisual:		elValorVisual;
		selectorActivo:	elSelectorActivo;
		valorActivo:		elValorActivo;
		yourself
	
"*VIPVersion 22-6-97 | 7:34:54 pm 'ACV'*"! !

!CMDefinedPartSlider class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartSlider class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesSlider!

metaPerspectivesSlider
	^OrderedCollection new
		addLast: ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Aspect' 'Change' 'Estilo')));
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsSlider!

metaSelectorsSlider

	"METAChildSpecAutoViewEditor openOn: self selector: #metaSelectors target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 12)
		add: (METATerminalChildSpec new
			name: 'Aspect';
			basicSelector: #aspect;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Aspect';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'Change';
			basicSelector: #change;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Change';
			displaySelector: nil;
			yourself);
		add: (METATerminalChildSpec new
			name: 'Estilo';
			basicSelector: #estilo;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Estilo';
			displaySelector: nil;
			yourself);
		yourself! !

!CMDefinedPartSlider publicMethodsFor: 'accessing'!

aspect
	^aspect
"*VIPVersion 22-6-97 | 7:34:52 pm 'ACV'*"!

aspect: elValor

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
	
	unValor = aspect  ifTrue: [ ^self].

	aspect := unValor.
	self markDirty.
	self changed: #aspect!

change
	^change
"*VIPVersion 22-6-97 | 7:34:52 pm 'ACV'*"!

change: elValor

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
	
	unValor = change  ifTrue: [ ^self].

	change := unValor.
	self markDirty.
	self changed: #change!

estilo
	^estilo
"*VIPVersion 22-6-97 | 7:34:52 pm 'ACV'*"!

estilo: elValor

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
	
	unValor = estilo  ifTrue: [ ^self].

	estilo := unValor.
	self markDirty.
	self changed: #estilo! !

!CMDefinedPartSlider publicMethodsFor: 'initialize-release'!

aspect: elAspect change: elChange estilo: elEstilo
	layoutSymbol: elLayoutSymbol layoutGroup: elLayoutGroup 

	aspect 		:= elAspect.
	change 		:= elChange.
	estilo		 := elEstilo.
	layoutSymbol := elLayoutSymbol.
	layoutGroup 	:= elLayoutGroup.
	^self
"*VIPVersion 22-6-97 | 7:34:53 pm 'ACV'*"!

initialize
	super initialize.


	aspect	:= nil.
	change	:= nil.
	estilo	:= nil.!

release
	aspect	:= nil.
	change	:= nil.
	estilo	:= nil.
	layoutSymbol	:= nil.
	layoutGroup	:= nil.
		super release

"*VIPVersion 22-6-97 | 7:34:53 pm 'ACV'*"! !

!CMDefinedPartSlider publicMethodsFor: 'interface opening'!

addDefinedTo: theDefinedEditor with: theBuilder  
	theDefinedEditor isNil ifTrue: [ ^nil].
	
	^theDefinedEditor addDefinedSlider: self with: theBuilder  
"*VIPVersion 6-7-97 | 10:38:34 pm 'ACV'*"! !

!CMDefinedPartSlider publicMethodsFor: 'persistence-code'!

initFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	self aspect: 				(theValues size < 13 ifTrue: [nil] ifFalse: [ theValues at: 13]). 
	self change: 				(theValues size < 14 ifTrue: [nil] ifFalse: [ theValues at: 14]).
	self estilo: 				(theValues size < 15 ifTrue: [nil] ifFalse: [ theValues at: 15]).!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  aspect); nextPutAll: aSep;  nextPutAll: (self pcForV:  self  change); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  estilo);  cr! !

!CMDefinedPartSlider publicMethodsFor: 'save defined parts'!

innerSaveForVisualOnStream: elStream
	super innerSaveForVisualOnStream: elStream.
	elStream nextPutAll: 
		self aspect printString, '	',
		self change printString, '	',
		self estilo printString, '	',
		self layoutSymbol printString, '	',
		self layoutGroup printString, '	'

"*VIPVersion 22-6-97 | 7:34:53 pm 'ACV'*"! !

!CMDefinedPartSwitch class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Conmutador'
"*VIPVersion 22-6-97 | 7:34:41 pm 'ACV'*"!

kind
	^#switch
"*VIPVersion 22-6-97 | 7:34:42 pm 'ACV'*"! !

!CMDefinedPartSwitch class publicMethodsFor: 'instance creation'!

named:								elNombre
	getSelector:						elGetSelector
	putSelector:						elPutSelector
	selectValue:						elSelectValue
	nlsSymbol: 						elNlsSymbol 
	nlsGroup: 						elNlsGroup
	layoutSymbol:					elLayoutSymbol
	layoutGroup:					elLayoutGroup
	selectorVisible:					elSelectorVisible
	valorVisible:						elValorVisible
	selectorVisual:					elSelectorVisual
	valorVisual:						elValorVisual
	selectorActivo:					elSelectorActivo
	valorActivo:						elValorActivo


	^(super named: elNombre)
		getSelector:						elGetSelector
		putSelector:						elPutSelector
		selectValue:						elSelectValue
		nlsSymbol: 						elNlsSymbol 
		nlsGroup: 							elNlsGroup
		layoutSymbol:						elLayoutSymbol
		layoutGroup:						elLayoutGroup;
		selectorVisible:					elSelectorVisible;
		valorVisible:						elValorVisible;
		selectorVisual:					elSelectorVisual;
		valorVisual:						elValorVisual;
		selectorActivo:					elSelectorActivo;
		valorActivo:						elValorActivo;
		yourself

"*VIPVersion 22-6-97 | 7:34:41 pm 'ACV'*"! !

!CMDefinedPartSwitch class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartSwitch class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesSwitch!

metaPerspectivesSwitch
	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('GetSelector' 'PutSelector' 'SelectValue' 'ValueExpression')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartSwitch_Perspectives';
			nlsItem: 'Specific';
			nlsTranslation: 'Especificos de Commutador';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'MetaInfo'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('MetaInfo' )))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartSwitch_Perspectives';
			nlsItem: 'MetaInfo';
			nlsTranslation: 'MetaInformacion';
			yourself);
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsSwitch!

metaSelectorsSwitch

	"METAChildSpecAutoViewEditor openOn: self selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 14)
		add: (METATerminalChildSpec new
			name: 'GetSelector';
			basicSelector: #getSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'GetSelector';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartSwitch_Selectors';
			nlsItem: 'GetSelector';
			nlsTranslation: 'SelectorLectura';
			yourself);
		add: (METATerminalChildSpec new
			name: 'PutSelector';
			basicSelector: #putSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'PutSelector';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartSwitch_Selectors';
			nlsItem: 'PutSelector';
			nlsTranslation: 'SelectorEscritura';
			yourself);
		add: (METATerminalChildSpec new
			name: 'SelectValue';
			basicSelector: #selectValue;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'SelectValue';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartSwitch_Selectors';
			nlsItem: 'SelectValue';
			nlsTranslation: 'ValorSeleccion';
			yourself);
		add: (METATerminalChildSpec new
			name: 'ValueExpression';
			basicSelector: #valueExpression;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ValueExpression';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartSwitch_Selectors';
			nlsItem: 'ValueExpression';
			nlsTranslation: 'ExpresionDeValor';
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MetaInfo';
			basicSelector: #metaInfo;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Select;
			helpString: 'MetaInfo';
			displaySelector: nil;
			objectClassName: #CODEElement;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			nlsApp: 'CODE';
			nlsGroup: 'DefinedPartSwitch_Selectors';
			nlsItem: 'MetaInfo';
			nlsTranslation: 'MetaInformacion';
			yourself);
		yourself! !

!CMDefinedPartSwitch class publicMethodsFor: 'persistence-code'!

sizeSymbolAtValue: theValue
	theValue = 30 ifTrue: [ ^#minSwitchHeight].
	theValue = 40 ifTrue: [ ^#midSwitchHeight].
	theValue = 50 ifTrue: [ ^#bigSwitchHeight].
	^nil!

sizeValueAtSymbol: theSymbol
	theSymbol = #minSwitchHeight ifTrue: [ ^30].
	theSymbol = #midSwitchHeight ifTrue: [ ^40].
	theSymbol = #bigSwitchHeight ifTrue: [ ^50].
	^nil! !

!CMDefinedPartSwitch publicMethodsFor: 'accessing'!

colorSolver
	^UserParameters colorSolverFor: #switch
"*VIPVersion 22-6-97 | 7:34:40 pm 'ACV'*"!

forzeGetSelector: elValor

	getSelector := elValor!

forzeMetaInfo: elValor
	metaInfo := elValor!

forzePutSelector: elValor

	putSelector := elValor!

forzeValueExpression: elValor
	valueExpression := elValor!

getSelector
	^getSelector.

"*VIPVersion 22-6-97 | 7:34:40 pm 'ACV'*"!

getSelector: elValor

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
	
	unValor = getSelector  ifTrue: [ ^self].

	getSelector := unValor.
	self markDirty.
	self changed: #getSelector!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo

	(theMetaInfo isKindOf: Array) ifTrue: [ 
		metaInfoRefTmpValues := theMetaInfo.
		^self
	].
	metaInfo := theMetaInfo.

	self markDirty.
	self changed: #metaInfo!

putSelector
	^putSelector.!

putSelector: elValor

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
	
	unValor = putSelector  ifTrue: [ ^self].

	putSelector := unValor.
	self markDirty.
	self changed: #putSelector!

selectValue
	^selectValue!

selectValue: elValor

	| unValor unosStrings aString |
	unValor := (elValor = true or: [ elValor = false or: [ elValor isKindOf: Number]])
		ifTrue: [ elValor]
		ifFalse: [ 
			(elValor isNil or: [ elValor isEmpty])
				ifTrue: [ nil] 
				ifFalse: [ 	
					unosStrings := elValor  asArrayOfSubstrings.
					unosStrings isEmpty
						ifTrue: [  nil]
						ifFalse: [ 
							aString := unosStrings size > 1  ifTrue: [ unosStrings first] ifFalse: [ elValor].
							aString = true printString ifTrue: [ true] ifFalse: [ 
							aString = false printString 	ifTrue: [ false] ifFalse: [
							aString first isDigit ifTrue: [ Number readFrom: aString readStream] ifFalse: [
							elValor]]]
						]
				]
		].

	unValor = selectValue  ifTrue: [ ^self].

	selectValue := unValor.
	self markDirty.
	self changed: #selectValue!

valueExpression
	^valueExpression!

valueExpression: elValor

	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ elValor]
		].
	
	unValor = valueExpression  ifTrue: [ ^self].

	valueExpression := unValor.
	self markDirty.
	self changed: #valueExpression! !

!CMDefinedPartSwitch publicMethodsFor: 'adaptors'!

buildValueAdaptorFrom: theDefinedEditor

	| anAdaptor |

	anAdaptor := self preferredCMValueAdaptor newForPart: self from: theDefinedEditor.
	^anAdaptor!

valueAdaptorFrom: theDefinedEditor

	| anAdaptor |

	theDefinedEditor isNil ifTrue: [ ^nil].

	anAdaptor := theDefinedEditor valueAdaptorForPart: self.
	^anAdaptor! !

!CMDefinedPartSwitch publicMethodsFor: 'initialize-release'!

getSelector:						elGetSelector
	putSelector:					elPutSelector
	selectValue:					elSelectValue
	nlsSymbol: 					elNlsSymbol 
	nlsGroup: 					elNlsGroup
	layoutSymbol:				elLayoutSymbol
	layoutGroup:				elLayoutGroup

	getSelector		:= elGetSelector.
	putSelector		:= elPutSelector.
	selectValue		:= elSelectValue.
	nlsSymbol			:= elNlsSymbol.
	nlsGroup			:= elNlsGroup.
	layoutSymbol		:= elLayoutSymbol.
	layoutGroup		:= elLayoutGroup.
	^self
"*VIPVersion 22-6-97 | 7:34:40 pm 'ACV'*"!

initialize
	super initialize.


	getSelector	:= nil.
	putSelector	:= nil.
	selectValue	:= nil.!

release
	getSelector	:= nil.
	putSelector	:= nil.
	selectValue	:= nil.
	super release! !

!CMDefinedPartSwitch publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	| aLayout aLabel  aRadioButtonSpec aLookPreferences aVisibilityAdaptor aEnablementAdaptor aValueAdaptor |

	aLayout := self hasLayoutFrame
		ifFalse: [ 
			self hasCooperativeLayout
				ifTrue: [ theDefinedEditor layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles]
				ifFalse: [ nil]
		]
		ifTrue: [ self layoutFrame ].

	aLayout isNil ifTrue: [ ^nil].

	aVisibilityAdaptor 		:= self visibilityAdaptorFrom: theDefinedEditor.
	aEnablementAdaptor 	:= self enablementAdaptorFrom: theDefinedEditor.

	aValueAdaptor := self valueAdaptorFrom: theDefinedEditor.
	aValueAdaptor isNil ifTrue: [ ^nil].

	aLookPreferences := UserParameters colorLookPreferencesFor: self kind.

	aLabel := (UpdatableString
		nlsSolver: 	theDefinedEditor 
		symbol:   	self nlsSymbol 
		group:    	self nlsGroup) actualString.

	aRadioButtonSpec := CMRadioButtonSpec new.
	aRadioButtonSpec
		name:  self name;
		layout: aLayout; 
		model: aValueAdaptor;
		hasImageOrientedLabel: false;
		setLabel: aLabel;
		select: self selectValue;
		colors: aLookPreferences;
		definedPart: self;
		definedEditor: theDefinedEditor;
		visibilityAdaptor: 				aVisibilityAdaptor;
		initiallyVisible:  			  		(self evaluateConstantPredicate: self initiallyVisiblePredicate);
		enablementAdaptor: 			aEnablementAdaptor
		initiallyEnabled:  				(self evaluateConstantPredicate: self initiallyEnabledPredicate).

	^aRadioButtonSpec



"*VIPVersion 11-7-97 | 8:25:11 pm 'ACV'*"! !

!CMDefinedPartSwitch publicMethodsFor: 'persistence-code'!

firstPersistenceIndexSwitch
	^self numberPersistenceEntriesDefinedPart + 
		self numberPersistenceEntriesWithLayoutRectangle + 
		self numberPersistenceEntriesWithNLS + 
		1!

forgetMetaInfo

	self metaInfo: nil.
	self changed: #metaInfo!

initFromValues: theValues
	
	| aFPI |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	aFPI := self firstPersistenceIndexSwitch.

	self forzeGetSelector: 			(theValues size < (aFPI + 0) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 0)]).
	self forzePutSelector: 			(theValues size < (aFPI + 1) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 1)]).
	self forzeSelectValue: 			(theValues size < (aFPI + 2) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 2)]).
	self forzeValueExpression: 	(theValues size < (aFPI + 3) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 3)]).

	theValues size < (aFPI + 4) ifFalse: [ self initReferencedMetaInfoFromValues: (theValues at: (aFPI + 4))]!

initMetaInfoRefTmpValuesFromModel 
	self metaInfo isNil ifTrue: [ ^self].

	metaInfoRefTmpValues := self metaInfo asReferenceArray!

initReferencedMetaInfoFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].

	theValues first = self class refToMetaInfoKindSymbol ifFalse: [ ^nil].

	metaInfoRefTmpValues := theValues.!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep aMetaInfo |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  getSelector );  nextPutAll: aSep; 
		nextPutAll: (self pcForV:  self  putSelector ); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  selectValue );  cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  valueExpression); cr.

	aMetaInfo := self metaInfo.
	aMetaInfo isNil  
		ifTrue: [ theStream nextPutAll: anIS; nextPutAll: (self pcForV: nil)] 
		ifFalse: [ aMetaInfo asReferenceAsCodeStringOn:  theStream indent: anIS]. 
	
	theStream cr.!

rebindReferencedMetaInfoValuesFromSolver: theSolver
	
	| someElements |

	(metaInfoRefTmpValues isNil or: [ metaInfoRefTmpValues isEmpty]) ifTrue: [ ^self].

	someElements := CODEElement resolveReferencedElementFromPersistenceAsCode: metaInfoRefTmpValues  
		solver: theSolver.
	self metaInfo: someElements.
	metaInfoRefTmpValues := nil!

rebindToModelFromSolver: theSolver

	self rebindReferencedMetaInfoValuesFromSolver: theSolver.!

unbindFromModel

	self initMetaInfoRefTmpValuesFromModel.! !

!CMDefinedPartTitle class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Titulo'
"*VIPVersion 22-6-97 | 7:35:11 pm 'ACV'*"!

kind
	^#title
"*VIPVersion 22-6-97 | 7:35:11 pm 'ACV'*"! !

!CMDefinedPartTitle class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 22-6-97 | 7:35:10 pm 'ACV'*"! !

!CMDefinedPartTitle class publicMethodsFor: 'instance creation'!

named: 								elNombre
	visibilityPredicate:				theVisibilityPredicate
	initiallyVisiblePredicate:  	theInitiallyVisiblePredicate
	enablementPredicate: 		theEnablementPredicate 
	initiallyEnabledPredicate:	theInitiallyEnabledPredicate
	layoutMode:					theLayoutMode
	layoutSymbol:					theLayoutSymbol
	layoutGroup:					theLayoutGroup
	layoutRectangle:				theLayoutRectangle
	layoutFrame: 					theLayoutFrame
	nlsSymbol: 						theNlsSymbol 
	nlsGroup: 						theNlsGroup
	enhance: 						theEnhance 
	justification:					theJustification
	borderWidth:					theBorderWidth

	| aDefinedPart |

	aDefinedPart := super named: elNombre.
	aDefinedPart
		visibilityPredicate:				theVisibilityPredicate;
		initiallyVisiblePredicate:  		theInitiallyVisiblePredicate;
		enablementPredicate: 			theEnablementPredicate ;
		initiallyEnabledPredicate:		theInitiallyEnabledPredicate;
		layoutMode:						theLayoutMode;
		layoutSymbol:					theLayoutSymbol;
		layoutGroup:						theLayoutGroup;
		layoutRectangle:				theLayoutRectangle;
		layoutFrame: 					theLayoutFrame;
		nlsSymbol:						theNlsSymbol;
		nlsGroup:							theNlsGroup;
		enhance:							theEnhance;
		justification:						theJustification;
		borderWidth:					theBorderWidth.

	^aDefinedPart! !

!CMDefinedPartTitle class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartTitle class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesTitle!

metaPerspectivesTitle
	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Enhance' 'Justification' 'BorderWidth')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartTitle_Perspectives';
			nlsItem: 'Specific';
			nlsTranslation: 'Especificos de Titulo';
			yourself);
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsTitle!

metaSelectorsTitle

	"METAChildSpecAutoViewEditor openOn: self selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 14)
		add: (METATerminalChildSpec new
			name: 'Enhance';
			basicSelector: #enhance;
			type: #Enum;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Enhance';
			displaySelector: nil;
			enumValuesString: 'normal bold italic underline serif sansserif bolditalic boldunderline boldunderlineitalic underlineitalic';
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartTitle_Selectors';
			nlsItem: 'Enhance';
			nlsTranslation: 'EnfasisLetras';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Justification';
			basicSelector: #justification;
			type: #Enum;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Justification';
			displaySelector: nil;
			enumValuesString: 'left right center';
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartTitle_Selectors';
			nlsItem: 'Justification';
			nlsTranslation: 'Justificacion';
			yourself);
		add: (METATerminalChildSpec new
			name: 'BorderWidth';
			basicSelector: #borderWidth;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'BorderWidth';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartTitle_Selectors';
			nlsItem: 'BorderWidth';
			nlsTranslation: 'GrosorBorde';
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MetaInfo';
			basicSelector: #metaInfo;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Select;
			helpString: 'MetaInfo';
			displaySelector: nil;
			objectClassName: #CODEElement;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			nlsApp: 'CODE';
			nlsGroup: 'DefinedPartTitle_Selectors';
			nlsItem: 'MetaInfo';
			nlsTranslation: 'MetaInformacion';
			yourself);
		yourself! !

!CMDefinedPartTitle class publicMethodsFor: 'persistence-code'!

sizeSymbolAtValue: theValue
	theValue = 30 ifTrue: [ ^#minTitleHeight].
	theValue = 40 ifTrue: [ ^#midTitleHeight].
	theValue = 50 ifTrue: [ ^#bigTitleHeight].
	^nil!

sizeValueAtSymbol: theSymbol
	theSymbol = #minTitleHeight ifTrue: [ ^30].
	theSymbol = #midTitleHeight ifTrue: [ ^40].
	theSymbol = #bigTitleHeight ifTrue: [ ^50].
	^nil! !

!CMDefinedPartTitle publicMethodsFor: 'accessing'!

borderWidth
	^borderWidth
"*VIPVersion 22-6-97 | 7:35:08 pm 'ACV'*"!

borderWidth: elValor

	| unValor unosStrings |
	unValor := (elValor isKindOf: Number)
		ifTrue: [ elValor]
		ifFalse: [ 
			(elValor isNil or: [ elValor isEmpty])
				ifTrue: [ false] 
				ifFalse: [ 	
					unosStrings := elValor  asArrayOfSubstrings.
					unosStrings isEmpty
						ifTrue: [  0]
						ifFalse: [ 
							unosStrings size > 1 
								ifTrue: [ Number readFrom: unosStrings first readStream]
								ifFalse: [ Number readFrom:  elValor]
						]
				]
		].
	
	unValor = borderWidth  ifTrue: [ ^self].

	borderWidth := unValor.
	self markDirty.
	self changed: #borderWidth!

enhance
	^enhance
"*VIPVersion 22-6-97 | 7:35:08 pm 'ACV'*"!

enhance: elValor

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
	
	unValor = enhance  ifTrue: [ ^self].

	enhance := unValor.
	self markDirty.
	self changed: #enhance!

forzeBorderWidth: elValor

	borderWidth := elValor!

forzeEnhance: elValor

	enhance := elValor!

forzeJustification: elValor
	justification := elValor!

forzeMetaInfo: theMetaInfo

	metaInfo := theMetaInfo.!

justification
	^justification
"*VIPVersion 22-6-97 | 7:35:08 pm 'ACV'*"!

justification: elValor

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
	
	unValor = justification  ifTrue: [ ^self].

	justification := unValor.
	self markDirty.
	self changed: #justification!

metaInfo
	^metaInfo!

metaInfo: theMetaInfo

	(theMetaInfo isKindOf: Array) ifTrue: [ 
		metaInfoRefTmpValues := theMetaInfo.
		^self
	].

	metaInfo := theMetaInfo.

	self markDirty.
	self changed: #metaInfo! !

!CMDefinedPartTitle publicMethodsFor: 'ayuda'!

mostrarEnAyudaBrowser
	^false
"*VIPVersion 22-6-97 | 7:35:10 pm 'ACV'*"! !

!CMDefinedPartTitle publicMethodsFor: 'initialize-release'!

initialize
	super initialize.


	enhance	:= #yourself.
	justification	:= #left.
	borderWidth	:= 0.!

release
	enhance	:= nil.
	justification	:= nil.
	borderWidth	:= nil.
	super release! !

!CMDefinedPartTitle publicMethodsFor: 'interface opening'!

builderSpecFrom: theDefinedEditor with: theBuilder withDefaultLayouts: theLayoutRectangles

	|  aLayout aLabel aLabelSpec aVisibilityAdaptor aEnablementAdaptor |

	aLayout := self hasLayoutFrame
		ifFalse: [ 
			self hasCooperativeLayout
				ifTrue: [ theDefinedEditor layoutFor: self layoutSymbol in: self layoutGroup  withDefaultLayouts: theLayoutRectangles]
				ifFalse: [ nil]
		]
		ifTrue: [ self layoutFrame ].

	aLayout isNil ifTrue: [ ^nil].

	aLabel := self translation asString.

	aVisibilityAdaptor 		:= self visibilityAdaptorFrom: theDefinedEditor.
	aEnablementAdaptor 	:= self enablementAdaptorFrom: theDefinedEditor.

	aLabelSpec := CMLabelSpec new.
	aLabelSpec
		name:  							self name;
		layout: 							aLayout; 
		hasImageOrientedLabel: 		false;
		setLabel: 							aLabel;
		definedPart: 						self;
		definedEditor: 					theDefinedEditor;
		visibilityAdaptor: 				aVisibilityAdaptor;
		initiallyVisible:  			  		(self evaluateConstantPredicate: self initiallyVisiblePredicate);
		enablementAdaptor: 			aEnablementAdaptor;
		initiallyEnabled:  				(self evaluateConstantPredicate: self initiallyEnabledPredicate).

	^aLabelSpec! !

!CMDefinedPartTitle publicMethodsFor: 'persistence-code'!

firstPersistenceIndexTitle
	^self numberPersistenceEntriesDefinedPart + 
		self numberPersistenceEntriesWithLayoutRectangle + 
		self numberPersistenceEntriesWithNLS + 
		1!

forgetMetaInfo

	self metaInfo: nil.
	self changed: #metaInfo!

initFromValues: theValues
	
	| aFPI |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	aFPI := self firstPersistenceIndexTitle.

	self forzeEnhance: 				(theValues size < (aFPI + 0) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 0)]).
	self forzeJustification: 			(theValues size < (aFPI + 1) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 1)]).
	self forzeBorderWidth: 		(theValues size < (aFPI + 2) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 2)]).

	theValues size < (aFPI + 3) ifFalse: [ self initReferencedMetaInfoFromValues: (theValues at: (aFPI + 3))]!

initMetaInfoRefTmpValuesFromModel 
	self metaInfo isNil ifTrue: [ ^self].

	metaInfoRefTmpValues := self metaInfo asReferenceArray!

initReferencedMetaInfoFromValues: theValues
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].
 
	theValues first = self class refToMetaInfoKindSymbol ifFalse: [ ^nil].

	metaInfoRefTmpValues := theValues.!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self enhance); nextPutAll: aSep;  
		nextPutAll: (self pcForV:  self justification); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self borderWidth); cr!

rebindReferencedMetaInfoValuesFromSolver: theSolver
	
	| someElements |

	(metaInfoRefTmpValues isNil or: [ metaInfoRefTmpValues isEmpty]) ifTrue: [ ^self].

	someElements := CODEElement resolveReferencedElementFromPersistenceAsCode: metaInfoRefTmpValues  
		solver: theSolver.
	self metaInfo: someElements.
	metaInfoRefTmpValues := nil!

rebindToModelFromSolver: theSolver

	self rebindReferencedMetaInfoValuesFromSolver: theSolver.!

unbindFromModel

	self initMetaInfoRefTmpValuesFromModel.! !

!CMDefinedPartTranscript class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Transcript'!

kind
	^#transcript! !

!CMDefinedPartTranscript class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 12-7-97 | 1:20:18 am 'ACV'*"! !

!CMDefinedPartTranscript class publicMethodsFor: 'instance creation'!

layoutSymbol:	elLayoutSymbol
	layoutGroup:		elLayoutGroup
	

	^super new
		layoutSymbol:	elLayoutSymbol;
		layoutGroup:	elLayoutGroup;
		yourself

"*VIPVersion 12-7-97 | 1:22:07 am 'ACV'*"!

named: elName
	layoutSymbol:	elLayoutSymbol
	layoutGroup:		elLayoutGroup
	

	^(super named: elName)
		layoutSymbol:	elLayoutSymbol;
		layoutGroup:	elLayoutGroup;
		yourself

"*VIPVersion 12-7-97 | 1:24:37 am 'ACV'*"! !

!CMDefinedPartTranscript class publicMethodsFor: 'menus'!

isCMDefinedPartFactory
	^true! !

!CMDefinedPartTranscript class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesTranscript!

metaPerspectivesTranscript
	^OrderedCollection new

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsTranscript!

metaSelectorsTranscript
	"METAChildSpecAutoViewEditor openOn: self selector: #metaSelectorsHere target: nil selector: nil."

	self ojoMETASelectors.
	^(OrderedCollection new: 9)
		yourself! !

!CMDefinedPartTranscript publicMethodsFor: 'initialize-release'!

release
	layoutSymbol	:= nil.
	layoutGroup	:= nil.
	super release
"*VIPVersion 12-7-97 | 1:14:39 am 'ACV'*"! !

!CMDefinedPartTranscript publicMethodsFor: 'save defined parts'!

innerSaveForVisualOnStream: elStream
	super innerSaveForVisualOnStream: elStream.
	elStream nextPutAll: 
		self layoutSymbol printString, '	',
		self layoutGroup printString, '	'
		

"*VIPVersion 12-7-97 | 1:17:03 am 'ACV'*"!

symbolInEditor: elEditor
	^elEditor nls: self nlsSymbol group: self nlsGroup

"*VIPVersion 12-7-97 | 1:17:21 am 'ACV'*"! !

!CMDefinedPartWithLayoutRectangle class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Boton'
"*VIPVersion 22-6-97 | 7:34:44 pm 'ACV'*"!

kind
	^#button
"*VIPVersion 22-6-97 | 7:34:44 pm 'ACV'*"! !

!CMDefinedPartWithLayoutRectangle class publicMethodsFor: 'accessing-layout'!

layoutName: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self layoutNameIndex ifTrue: [ ^nil].
	theLayoutRectangle at: self layoutNameIndex put: theValue!

layoutNameFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self layoutNameIndex ifTrue: [ ^nil].
	^theLayoutRectangle at: self layoutNameIndex!

layoutNameIndex
	^1!

maxExtentIndex
	^4!

maxExtentPoint: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self maxExtentIndex ifTrue: [ ^nil].
	theLayoutRectangle at: self maxExtentIndex put: theValue!

maxExtentPointFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self maxExtentIndex ifTrue: [ ^nil].
	^theLayoutRectangle at: self maxExtentIndex!

maxExtentX: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self maxExtentIndex ifTrue: [ ^nil].
	(theLayoutRectangle at: self maxExtentIndex) x: theValue!

maxExtentXFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self maxExtentIndex ifTrue: [ ^nil].
	^(theLayoutRectangle at: self maxExtentIndex) x!

maxExtentY: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self maxExtentIndex ifTrue: [ ^nil].
	(theLayoutRectangle at: self maxExtentIndex) y: theValue!

maxExtentYFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self maxExtentIndex ifTrue: [ ^nil].
	^(theLayoutRectangle at: self maxExtentIndex) y!

minExtentIndex
	^3!

minExtentX: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self minExtentIndex ifTrue: [ ^nil].
	(theLayoutRectangle at: self minExtentIndex) x: theValue!

minExtentXFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self minExtentIndex ifTrue: [ ^nil].
	^(theLayoutRectangle at: self minExtentIndex) x!

minExtentY: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self minExtentIndex ifTrue: [ ^nil].
	(theLayoutRectangle at: self minExtentIndex) y: theValue!

minExtentYFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self minExtentIndex ifTrue: [ ^nil].
	^(theLayoutRectangle at: self minExtentIndex) y!

mustAdvanceRow: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self mustAdvanceRowIndex ifTrue: [ ^nil].
	theLayoutRectangle at: self mustAdvanceRowIndex put: theValue!

mustAdvanceRowFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self mustAdvanceRowIndex ifTrue: [ ^nil].
	^theLayoutRectangle at: self mustAdvanceRowIndex!

mustAdvanceRowIndex
	^6!

relativeHeight: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self relativeHeightIndex ifTrue: [ ^nil].
	theLayoutRectangle at: self relativeHeightIndex put: theValue!

relativeHeightFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [^nil].
	theLayoutRectangle size < self relativeHeightIndex ifTrue: [ ^nil].
	^theLayoutRectangle at: self relativeHeightIndex!

relativeHeightIndex
	^2!

relXIndex
	^5!

relXStart: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [ ^nil].
	theLayoutRectangle size < self relXIndex ifTrue: [  ^nil].
	(theLayoutRectangle at: self relXIndex) at: 1 put: theValue!

relXStartFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [ ^nil].
	theLayoutRectangle size < self relXIndex ifTrue: [  ^nil].
	^(theLayoutRectangle at: self relXIndex) at: 1!

relXWidth: theValue into: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [ ^nil].
	theLayoutRectangle size < self relXIndex ifTrue: [  ^nil].
	(theLayoutRectangle at: self relXIndex) at: 2 put: theValue!

relXWidthFrom: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [ ^nil].
	theLayoutRectangle size < self relXIndex ifTrue: [  ^nil].
	^(theLayoutRectangle at: self relXIndex) at: 2! !

!CMDefinedPartWithLayoutRectangle class publicMethodsFor: 'generating'!

aceptaGeneratedAccessServices
	^true
"*VIPVersion 22-6-97 | 7:34:44 pm 'ACV'*"! !

!CMDefinedPartWithLayoutRectangle class publicMethodsFor: 'instance creation'!

named:								elNombre
	actionSelector:					elActionSelector
	nlsSymbol: 						elNlsSymbol 
	nlsGroup: 						elNlsGroup
	layoutSymbol:					elLayoutSymbol
	layoutGroup:					elLayoutGroup
	selectorVisible:					elSelectorVisible
	valorVisible:						elValorVisible
	selectorVisual:					elSelectorVisual
	valorVisual:						elValorVisual
	selectorActivo:					elSelectorActivo
	valorActivo:						elValorActivo


	^(super named: elNombre)
		actionSelector:					elActionSelector
		nlsSymbol: 						elNlsSymbol 
		nlsGroup: 							elNlsGroup
		layoutSymbol:						elLayoutSymbol
		layoutGroup:						elLayoutGroup;
		selectorVisible:					elSelectorVisible;
		valorVisible:						elValorVisible;
		selectorVisual:					elSelectorVisual;
		valorVisual:						elValorVisual;
		selectorActivo:					elSelectorActivo;
		valorActivo:						elValorActivo;
		yourself

"*VIPVersion 22-6-97 | 7:34:43 pm 'ACV'*"! !

!CMDefinedPartWithLayoutRectangle class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesWithLayoutRectangle!

metaPerspectivesWithLayoutRectangle

	^OrderedCollection new
	
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'LayoutRectangle'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('LayoutMode' 'LayoutSymbol' 'LayoutGroup' 'RelativeHeight' 'MinExtentX' 'MinExtentY' 'MaxExtentX' 'MaxExtentY' 'RelXStart' 'RelXWidth' 'MustAdvanceRow')))
			nlsApp: 'CMDF';
			nlsGroup: 'WithLayoutRectangle_Perspectives';
			nlsItem: 'LayoutRectangle';
			nlsTranslation: 'FormatoCooperativo';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'LayoutFrame'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('LayoutMode' 'TopFraction' 'TopOffset' 'LeftFraction' 'LeftOffset' 'BottomFraction' 'BottomOffset' 'RightFraction' 'RightOffset' )))
			nlsApp: 'CMDF';
			nlsGroup: 'WithLayoutRectangle_Perspectives';
			nlsItem: 'LayoutFrame';
			nlsTranslation: 'FomatoFijo';
			yourself);
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsWithLayoutRectangle!

metaSelectorsWithLayoutRectangle

	"METAChildSpecAutoViewEditor openOn: CMDefinedPart selector: #metaSelectorsWithLayoutRectangle target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 2)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LayoutMode';
			basicSelector: #layoutMode;
			type: #Enum;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LayoutMode';
			displaySelector: nil;
			canShowInTree: true;
			enumValuesString: 'cooperative fixed';
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'LayoutMode';
			nlsTranslation: 'ModoFormato';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LayoutSymbol';
			basicSelector: #layoutSymbol;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LayoutSymbol';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'LayoutSymbol';
			nlsTranslation: 'SimboloLayout';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LayoutGroup';
			basicSelector: #layoutGroup;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LayoutGroup';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'LayoutGroup';
			nlsTranslation: 'GrupoLayout';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RelativeHeight';
			basicSelector: #relativeHeight;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RelativeHeight';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'RelativeHeight';
			nlsTranslation: 'AlturaRelativa';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MinExtentX';
			basicSelector: #minExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentX';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'MinExtentX';
			nlsTranslation: 'MinimaExtensionX';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MinExtentY';
			basicSelector: #minExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentY';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'MinExtentY';
			nlsTranslation: 'MinimaExtensionY';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MaxExtentX';
			basicSelector: #maxExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentX';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'MaxExtentX';
			nlsTranslation: 'MaximaExtensionX';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MaxExtentY';
			basicSelector: #maxExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentY';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'MaxExtentY';
			nlsTranslation: 'MaximaExtensionY';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RelXStart';
			basicSelector: #relXStart;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RelXStart';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'RelXStart';
			nlsTranslation: 'ComienzoRelativoX';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RelXWidth';
			basicSelector: #relXWidth;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RelXWidth';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'RelXWidth';
			nlsTranslation: 'AmplitudRelativaX';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MustAdvanceRow';
			basicSelector: #mustAdvanceRow;
			type: #Boolean;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MustAdvanceRow';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'MustAdvanceRow';
			nlsTranslation: 'DebeAvanzarLinea';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'TopFraction';
			basicSelector: #topFraction;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TopFraction';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'TopFraction';
			nlsTranslation: 'FractionSuperior';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'TopOffset';
			basicSelector: #topOffset;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TopOffset';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'TopOffset';
			nlsTranslation: 'DesplazamientoSuperior';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LeftFraction';
			basicSelector: #leftFraction;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LeftFraction';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'LeftFraction';
			nlsTranslation: 'FraccionIzquierda';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LeftOffset';
			basicSelector: #leftOffset;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LeftOffset';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'LeftOffset';
			nlsTranslation: 'DesplazamientoIzquierda';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'BottomFraction';
			basicSelector: #bottomFraction;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'BottomFraction';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'BottomFraction';
			nlsTranslation: 'FraccionInferior';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'BottomOffset';
			basicSelector: #bottomOffset;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'BottomOffset';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'BottomOffset';
			nlsTranslation: 'DesplazamientoInferior';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RightFraction';
			basicSelector: #rightFraction;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RightFraction';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'RightFraction';
			nlsTranslation: 'FraccionDerecha';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RightOffset';
			basicSelector: #rightOffset;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RightOffset';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'RightOffset';
			nlsTranslation: 'DesplazamientoDerecho';
			yourself);
		yourself! !

!CMDefinedPartWithLayoutRectangle class publicMethodsFor: 'persistence-code'!

sizeSymbolAtValue: theValue
	^nil! !

!CMDefinedPartWithLayoutRectangle publicMethodsFor: 'accessing'!

bottomFraction
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame bottomFraction]!

bottomOffset
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame bottomOffset]!

forzeLayoutFrame: theLayoutFrame
	layoutFrame := theLayoutFrame!

forzeLayoutGroup: theLayoutGroup
	layoutGroup := theLayoutGroup!

forzeLayoutMode: theLayoutMode
	layoutMode := theLayoutMode!

forzeLayoutRectangle: theLayoutRectangle
	layoutRectangle := theLayoutRectangle!

forzeLayoutSymbol: theLayoutSymbol
	layoutSymbol := theLayoutSymbol!

layoutFrame
	^layoutFrame!

layoutFrame: elValor

	
	elValor = layoutFrame  ifTrue: [ ^self].

	layoutFrame := elValor.
	self markDirty.
	self changed: #layoutFrame!

layoutGroup
	^layoutGroup!

layoutGroup: elValor

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
	
	unValor = layoutGroup  ifTrue: [ ^self].

	layoutGroup := unValor.
	self markDirty.
	self changed: #layoutGroup!

layoutMode
	^layoutMode!

layoutMode: elValor

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
	
	unValor = layoutMode  ifTrue: [ ^self].

	layoutMode := unValor.
	self markDirty.
	self changed: #layoutMode!

layoutRectangle
	^layoutRectangle!

layoutRectangle: theLayoutRectangle
	| aPrevLayoutRectangle |

	aPrevLayoutRectangle := layoutRectangle.

	layoutRectangle := theLayoutRectangle.

	aPrevLayoutRectangle isNil ifFalse: [ 
		self container isNil ifFalse: [ self container unregisterLayoutRectangle:  aPrevLayoutRectangle]
	].

	self container isNil ifFalse: [ self container registerNewLayoutRectangle:  theLayoutRectangle]!

layoutSymbol
	^layoutSymbol!

layoutSymbol: elValor

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
	
	unValor = layoutSymbol  ifTrue: [ ^self].

	layoutSymbol := unValor.
	self markDirty.

	self layoutName: layoutSymbol.

	self changed: #layoutSymbol! !

!CMDefinedPartWithLayoutRectangle publicMethodsFor: 'accessing-layout frame'!

leftFraction
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame leftFraction]!

leftOffset
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame leftOffset]!

rightFraction
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame rightFraction]!

rightOffset
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame rightOffset]!

topFraction
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame topFraction]!

topOffset
	^layoutFrame isNil ifTrue: [nil] ifFalse: [ layoutFrame topOffset]! !

!CMDefinedPartWithLayoutRectangle publicMethodsFor: 'accessing-layout rectangle'!

layoutName
	layoutRectangle isNil ifTrue: [^nil].
	^self class layoutNameFrom: layoutRectangle.!

layoutName: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class layoutName: theValue into: layoutRectangle.
	self changed: #layoutName!

maxExtentPoint
	layoutRectangle isNil ifTrue: [^nil].
	^self class maxExtentPointFrom: layoutRectangle.!

maxExtentPoint: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class maxExtentPoint: theValue into: layoutRectangle.
	self changed: #maxExtentPoint!

maxExtentX
	layoutRectangle isNil ifTrue: [^nil].
	^self class maxExtentXFrom: layoutRectangle.!

maxExtentX: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class maxExtentX: theValue into: layoutRectangle.
	self changed: #maxExtentX!

maxExtentY
	layoutRectangle isNil ifTrue: [^nil].
	^self class maxExtentYFrom: layoutRectangle.!

maxExtentY: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class maxExtentY: theValue into: layoutRectangle.
	self changed: #maxExtentY!

minExtentPoint
	layoutRectangle isNil ifTrue: [^nil].
	layoutRectangle size < 3 ifTrue: [ ^nil].
	^layoutRectangle at: 3!

minExtentPoint: theValue
	layoutRectangle isNil ifTrue: [^self].
	layoutRectangle size < 3 ifTrue: [ ^self].
	layoutRectangle at: 3 put: theValue.
	self changed: #minExtentPoint!

minExtentX
	layoutRectangle isNil ifTrue: [^nil].
	^self class minExtentXFrom: layoutRectangle.!

minExtentX: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class minExtentX: theValue into: layoutRectangle.
	self changed: #minExtentX!

minExtentY
	layoutRectangle isNil ifTrue: [^nil].
	^self class minExtentYFrom: layoutRectangle.!

minExtentY: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class minExtentY: theValue into: layoutRectangle.
	self changed: #minExtentY!

mustAdvanceRow
	layoutRectangle isNil ifTrue: [^nil].
	^self class mustAdvanceRowFrom: layoutRectangle.!

mustAdvanceRow: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class mustAdvanceRow: theValue into: layoutRectangle.
	self changed: #mustAdvanceRow!

relativeHeight
	layoutRectangle isNil ifTrue: [^nil].
	^self class relativeHeightFrom: layoutRectangle.!

relativeHeight: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class relativeHeight: theValue into: layoutRectangle.
	self changed: #relativeHeight!

relXStart
	layoutRectangle isNil ifTrue: [^nil].
	^self class relXStartFrom: layoutRectangle.!

relXStart: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class relXStart: theValue into: layoutRectangle.
	self changed: #relXStart!

relXWidth
	layoutRectangle isNil ifTrue: [^nil].
	^self class relXWidthFrom: layoutRectangle.!

relXWidth: theValue

	layoutRectangle isNil ifTrue: [^self].
	self class relXWidth: theValue into: layoutRectangle.
	self changed: #relXWidth! !

!CMDefinedPartWithLayoutRectangle publicMethodsFor: 'initialize-release'!

initialize
	super initialize.

	layoutSymbol	:= nil.
	layoutGroup	:= nil.
	layoutMode := self class layoutModeCooperativeSymbol.
	layoutFrame := nil.

	self initLayoutRectangle!

initLayoutMode
		
	layoutMode := self class layoutModeCooperativeSymbol!

initLayoutRectangle
	"name, relative height, min extent point , max extent point, #(relXStart relXWidth) , must advance row"		
 
	layoutRectangle := 
		Array 
			with: #sep
			with: 1/10
		 	with: 0@30
			with: 0@50 
			with: (Array with: 0.12 with: 0.35) 
			with: false!

layoutSymbol:		elLayoutSymbol layoutGroup:		elLayoutGroup

	layoutSymbol		:= elLayoutSymbol.
	layoutGroup		:= elLayoutGroup.!

release
	layoutSymbol	:= nil.
	layoutGroup		:= nil.
	super release! !

!CMDefinedPartWithLayoutRectangle publicMethodsFor: 'layout'!

initLayoutRectangleFromIdentical: theLCA
	"name, relative height, min extent point , max extent point, #(relXStart relXWidth) , must advance row"		
(theLCA isKindOf: Array) ifFalse: [ self halt].

	layoutRectangle := theLCA! !

!CMDefinedPartWithLayoutRectangle publicMethodsFor: 'persistence-code'!

firstPersistenceIndexWithLayoutRectangle
	^self numberPersistenceEntriesDefinedPart + 1!

initFromValues: theValues
	
	| aFPI |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	aFPI := self firstPersistenceIndexWithLayoutRectangle.

	self layoutMode: 		(theValues size < (aFPI + 0) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 0)]). 
	self layoutSymbol: 		(theValues size < (aFPI + 1) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 1)]). 
	self layoutGroup: 		(theValues size < (aFPI + 2) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 2)]).
				
	theValues size < (aFPI + 3) ifFalse: [ self initLayoutRectangleFromValues: (theValues at: (aFPI + 3))].
	theValues size < (aFPI + 4) ifFalse: [ self initLayoutFrameFromValues: (theValues at: (aFPI + 4))].!

initLayoutFrameFromValues: theValues
	
	| aLayoutFrame |

	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].
	
	aLayoutFrame := LayoutFrame new
		topFraction: 			(theValues size < 2 ifTrue: [nil] ifFalse: [ theValues at: 2]);
		topOffset: 			(theValues size < 3 ifTrue: [nil] ifFalse: [ theValues at: 3]);
		leftFraction: 			(theValues size < 4 ifTrue: [nil] ifFalse: [ theValues at: 4]);
		leftOffset: 			(theValues size < 5 ifTrue: [nil] ifFalse: [ theValues at: 5]);
		bottomFraction: 		(theValues size < 6 ifTrue: [nil] ifFalse: [ theValues at: 6]);
		bottomOffset: 		(theValues size < 7 ifTrue: [nil] ifFalse: [ theValues at: 7]);
		rightFraction: 		(theValues size < 8 ifTrue: [nil] ifFalse: [ theValues at: 8]);
		rightOffset: 			(theValues size < 9 ifTrue: [nil] ifFalse: [ theValues at: 9]);
		yourself.

	layoutFrame := aLayoutFrame.!

initLayoutRectangleFromValues: theValues
	
	| aLayoutRectangle |


	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].
	
	aLayoutRectangle := Array
		with: self layoutSymbol
		with: (theValues size < 2 ifTrue: [nil] ifFalse: [ theValues at: 2])
		with: (theValues size < 3 ifTrue: [nil] ifFalse: [ (theValues at: 3) first @ ((theValues at: 3) at: 2) ])
		with: (theValues size < 4 ifTrue: [nil] ifFalse: [ (theValues at: 4) first @ ((theValues at: 4) at: 2) ])
		with: (theValues size < 5 ifTrue: [nil] ifFalse: [ Array with: (theValues at: 5) first with: ((theValues at: 5) at: 2) ])
		with: (theValues size < 6 ifTrue: [nil] ifFalse: [ theValues at: 6]).

	layoutRectangle := aLayoutRectangle.

	self container isNil ifFalse: [ self container registerNewLayoutRectangle:  aLayoutRectangle]!

layoutFramePersistenceAsCodeStringOn: theStream indent: theIS

	| anIS |
	theStream isNil ifTrue:  [^nil].

	anIS := theIS , self indentStringForPersistenceAsCode.

	(self topFraction isNil or: [ self topFraction = 0]) & (self topOffset isNil or: [ self topOffset = 0]) &
	(self leftFraction isNil or: [ self leftFraction = 0]) & (self leftOffset isNil or: [ self leftOffset = 0]) &
	(self bottomFraction isNil or: [ self bottomFraction = 0]) & (self bottomOffset isNil or: [ self bottomOffset = 0]) &
	(self rightFraction isNil or: [ self rightFraction = 0]) & (self rightOffset isNil or: [ self rightOffset = 0]) ifTrue: [ 
		theStream nextPutAll: (self pcForV: nil); cr.
		^self
	].

	theStream
		nextPutAll: '( ';  nextPutAll: (self pcForV: self class layoutFramePersistenceSymbol);  space;
		nextPutAll: (self pcForV: self topFraction); space;
		nextPutAll: (self pcForV: self topOffset); space;
		nextPutAll: (self pcForV: self leftFraction); space;
		nextPutAll: (self pcForV: self leftOffset); space;
		nextPutAll: (self pcForV: self bottomFraction); space;
		nextPutAll: (self pcForV: self bottomOffset); space;
		nextPutAll: (self pcForV: self rightFraction); space;
		nextPutAll: (self pcForV: self rightOffset); space;
		nextPutAll: ')'; cr!

layoutRectanglePersistenceAsCodeStringOn: theStream indent: theIS

	| anIS |
	theStream isNil ifTrue:  [^nil].

	anIS := theIS , self indentStringForPersistenceAsCode.
	theStream nextPutAll: anIS; nextPutAll: '( ';  nextPutAll: (self pcForV: self class layoutRectanglePersistenceSymbol);  space;
		nextPutAll: (self pcForV: self relativeHeight); space;
		nextPutAll: '('; nextPutAll: (self pcForV: self minExtentX); space; 
		nextPutAll: (self pcForV: self minExtentY) ; nextPutAll: ' ) ';
		nextPutAll: '('; nextPutAll: (self pcForV: self maxExtentX); space; 
		nextPutAll: (self pcForV: self maxExtentY); nextPutAll: ' ) ';
		nextPutAll: '( '; nextPutAll: (self pcForV: self relXStart); space; 
		nextPutAll: (self pcForV: self relXWidth); nextPutAll: ' ) ';
		nextPutAll: (self mustAdvanceRow == true) printString; space;
		nextPutAll: ')'; cr.!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  layoutMode );  nextPutAll: aSep; 
		nextPutAll: (self pcForV:  self  layoutSymbol ); nextPutAll: aSep; 
		nextPutAll: (self pcForV:  self  layoutGroup ); cr.

	self layoutRectanglePersistenceAsCodeStringOn: theStream indent: theIS.
	self layoutFramePersistenceAsCodeStringOn: theStream indent: theIS!

numberPersistenceEntriesWithLayoutRectangle
	^5!

pcSizeVarForV: theValue

	| aValue |
	theValue isNil ifTrue: [ ^self pcForV: theValue].

	aValue := self class sizeSymbolAtValue: theValue.
	aValue isNil ifTrue: [ ^self pcForV: theValue].

	^self pcForV: aValue! !

!CMDefinedPartWithLayoutRectangle publicMethodsFor: 'tests'!

hasCooperativeLayout
	^self layoutMode = self class layoutModeCooperativeSymbol!

hasLayoutFrame
	^self layoutMode = self class layoutModeFixedSymbol and: [ self layoutFrame notNil]!

hasLayoutRectangle
	^true! !

!CMDefinedPartWithNLS class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'WithNLS'!

kind
	^#withNLS! !

!CMDefinedPartWithNLS class publicMethodsFor: 'instance creation'!

named:								elNombre
	nlsSymbol: 						elNlsSymbol 
	nlsGroup: 						elNlsGroup
	layoutSymbol:					elLayoutSymbol
	layoutGroup:					elLayoutGroup
	selectorVisible:				elSelectorVisible
	valorVisible:					elValorVisible
	selectorVisual:					elSelectorVisual
	valorVisual:						elValorVisual
	selectorActivo:					elSelectorActivo
	valorActivo:						elValorActivo


	^(super named: elNombre)
		nlsSymbol: 						elNlsSymbol 
		nlsGroup: 						elNlsGroup
		layoutSymbol:					elLayoutSymbol
		layoutGroup:						elLayoutGroup
		selectorVisible:					elSelectorVisible
		valorVisible:						elValorVisible
		selectorVisual:					elSelectorVisual
		valorVisual:						elValorVisual
		selectorActivo:					elSelectorActivo
		valorActivo:						elValorActivo
		yourself! !

!CMDefinedPartWithNLS class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesWithNLS!

metaPerspectivesWithNLS
	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Native Language Suport'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('DefaultTranslation' 'NlsSymbol' 'NlsGroup' 'Translation' 'ItemTranslation')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartButton_Perspectives';
			nlsItem: 'Native Language Suport';
			nlsTranslation: 'Traducciones';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'ItemTranslation'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('NLSItem')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedPartButton_Perspectives';
			nlsItem: 'Specific';
			nlsTranslation: 'ElementoDeTraduccion';
			yourself);
		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsWithNLS!

metaSelectorsWithNLS

	"METAChildSpecAutoViewEditor openOn: CMDefinedPart selector: #metaSelectorsForNLSTranslation target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 2)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DefaultTranslation';
			basicSelector: #defaultTranslation;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DefaultTranslation';
			displaySelector: nil;
			canShowInTree: false;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithLayoutRectangle_Selectors';
			nlsItem: 'DefaultTranslation';
			nlsTranslation: 'TraduccionPorDefecto';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'NlsSymbol';
			basicSelector: #nlsSymbol;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'NlsSymbol';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithNLS_Selectors';
			nlsItem: 'NlsSymbol';
			nlsTranslation: 'SimboloTraduccion';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'NlsGroup';
			basicSelector: #nlsGroup;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'NlsGroup';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithNLS_Selectors';
			nlsItem: 'NlsGroup';
			nlsTranslation: 'GrupoTraduccion';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Translation';
			basicSelector: #translation;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'NLS';
			displaySelector: nil;
			canShowInTree: false;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithNLS_Selectors';
			nlsItem: 'Translation';
			nlsTranslation: 'Traduccion';
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ItemTranslation';
			basicSelector: #itemTranslation;
			type: #Object;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ItemTranslation';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #ItemTranslation;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithNLS_Selectors';
			nlsItem: 'ItemTranslation';
			nlsTranslation: 'ElementoTraduccion';
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'NLSSolver';
			basicSelector: #nlsSolver;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Select;
			helpString: 'NLSSolver';
			displaySelector: nil;
			objectClassName: #TranslationModelBase;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartWithNLS_Selectors';
			nlsItem: 'NLSSolver';
			nlsTranslation: 'AplicacionOCatalogoDeTraduccion';
			yourself);
		yourself! !

!CMDefinedPartWithNLS publicMethodsFor: 'accessing'!

defaultTranslation
	^defaultTranslation!

defaultTranslation: elValor

	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [elValor]
		].
	
	unValor = defaultTranslation  ifTrue: [ ^self].

	defaultTranslation := unValor.
	self markDirty.
	self changed: #defaultTranslation!

forzeDefaultTranslation: elValor

	defaultTranslation := elValor.!

forzeNLSGroup: elValor
	nlsGroup := elValor!

forzeNLSSymbol: elValor
	nlsSymbol := elValor!

nlsGroup
	^nlsGroup!

nlsGroup: elValor

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
	
	unValor = nlsGroup  ifTrue: [ ^self].

	nlsGroup := unValor.
	self markDirty.
	self changed: #nlsGroup!

nlsSymbol
	^nlsSymbol!

nlsSymbol: elValor

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
	
	unValor = nlsSymbol  ifTrue: [ ^self].

	nlsSymbol := unValor.
	self markDirty.
	self changed: #nlsSymbol! !

!CMDefinedPartWithNLS publicMethodsFor: 'initialize-release'!

initialize
	super initialize.


	nlsSymbol	:= nil.
	nlsGroup	:= nil.
	defaultTranslation	:= nil.!

nlsSymbol:  elNlsSymbol  nlsGroup: elNlsGroup

	nlsSymbol			:= elNlsSymbol.
	nlsGroup			:= elNlsGroup.!

release
	nlsSymbol	:= nil.
	nlsGroup	:= nil.
	defaultTranslation	:= nil.		
	super release! !

!CMDefinedPartWithNLS publicMethodsFor: 'nls'!

itemTranslation

	| aNLSSolver aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	aNLSItem := aNLSSolver nlsLocalResolverItemAppNoDefault: self nlsAppName group: self nlsGroup item: self nlsSymbol.
	^aNLSItem!

itemTranslationLinkCreate

	| aNLSSolver aNLSItem |

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	aNLSItem := aNLSSolver nlsLocalResolverItemOrCreateApp: self nlsAppName group: self nlsGroup
		item: self nlsSymbol translation: self defaultTranslation.

	self changed: #itemTranslation.
	^aNLSItem!

nlsAppName
	| aTopPart |
	
	aTopPart := self topPart.
	aTopPart isNil ifTrue: [ ^nil].

	^aTopPart nlsAppName!

nlsResolverItem

	| aNLSSolver aNLSItem |
	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifTrue: [ ^nil].

	aNLSItem := aNLSSolver nlsResolverItemAppNoDefault: self nlsAppName group: self nlsGroup item: self nlsSymbol.
	^aNLSItem!

translation
	
	| aResolverItem aResult aDefaultTranslation |
	aDefaultTranslation :=  self defaultTranslation isNil ifTrue: [ self nlsSymbol] ifFalse: [ self defaultTranslation].

	aResolverItem := self nlsResolverItem.
	aResolverItem isNil ifTrue: [ ^aDefaultTranslation].
	aResult := aResolverItem translation.
	aResult isNil ifTrue: [ ^aDefaultTranslation].

	^aResult!

translation: theNewTranslation
	
	| aResolverItem aResult aTopPart anEditorClassName aGroup anItem someSameClassModels |

	theNewTranslation isNil ifTrue: [ ^self].

	self defaultTranslation: theNewTranslation.

	aResolverItem := self nlsResolverItem.
	aResolverItem isNil ifTrue: [ ^self].

	aResult := aResolverItem translation .
	aResult isNil ifFalse: [ aResult asString = theNewTranslation ifTrue: [ ^self ]].

	aResolverItem translation: theNewTranslation.

	aTopPart := self topPart.
	aTopPart isFromEditorClass ifFalse: [ ^nil].

	anEditorClassName := aTopPart editorClassName asString.
	anEditorClassName isNil ifTrue: [ anEditorClassName := aTopPart storeClassName, ' class'].
	anEditorClassName isNil ifTrue: [ ^nil].

	aGroup := self nlsGroup.
	aGroup isNil ifTrue: [ ^nil].
	anItem := self nlsSymbol.
	anItem isNil ifTrue: [ ^nil].

	someSameClassModels := InterModelUpdater interModelUpdater models select: [:aModel |
		aModel class name asString = anEditorClassName
	].
	someSameClassModels do: [:aModel | aModel change: theNewTranslation nls: anItem group: aGroup].

	self changed: #nls.
	self changed: #nlsResolverItem.	
	^aResult! !

!CMDefinedPartWithNLS publicMethodsFor: 'persistence-code'!

firstPersistenceIndexWithNLS
	^self numberPersistenceEntriesDefinedPart + 
		self numberPersistenceEntriesWithLayoutRectangle + 
		1!

initFromValues: theValues
	
	| aFPI |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.

	aFPI := self firstPersistenceIndexWithNLS.

	self forzeNLSSymbol: 			(theValues size < (aFPI + 0) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 0)]). 
	self forzeNLSGroup: 			(theValues size < (aFPI + 1) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 1)]).
	self forzeDefaultTranslation: 	(theValues size < (aFPI + 2) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 2)])!

localValuesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |
	theStream isNil ifTrue: [ ^self]. 

	super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.

	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	theStream 
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  nlsSymbol );  nextPutAll: aSep; 
		nextPutAll: (self pcForV:  self  nlsGroup ); cr;
		nextPutAll: anIS;  nextPutAll: (self pcForV:  self  defaultTranslation ); cr!

numberPersistenceEntriesWithNLS
	^3! !

!CMDefinedPartWithNLS publicMethodsFor: 'tests'!

isNLS
	^true! !

!CMDefinedPartXYSlider class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Deslizador XY'
"*VIPVersion 22-6-97 | 7:34:54 pm 'ACV'*"!

kind

	^#xyslider
"*VIPVersion 22-6-97 | 7:34:54 pm 'ACV'*"! !

!CMDefinedWindow class publicMethodsFor: 'accessing'!

claseCMDefinedPart
	^'Navegador' copy!

kind
	^#window! !

!CMDefinedWindow class publicMethodsFor: 'instance creation'!

named: 									elName 
	titleDefaultTranslation:			theTitleDefaultTranslation
	titleNlsSymbol: 					elTitleNlsSymbol
	titleNlsGroup: 						elTitleNlsGroup 
	origin: 								elOrigin
	minExtent:							elMinExtent
	prefExtent:							elPrefExtent
	maxExtent:							elMaxExtent
	storeClassName:					elStoreClassName
	storeMethodSelector:			elStoreMethodSelector
	parts: 								theParts 

	| aDefinedWindow |
	aDefinedWindow := super new name: elName.
	aDefinedWindow
		titleDefaultTranslation:				theTitleDefaultTranslation;
		titleNlsSymbol: 						elTitleNlsSymbol;
		titleNlsGroup: 						elTitleNlsGroup;
		minExtent:							elMinExtent;
		prefExtent:							elPrefExtent;
		maxExtent:							elMaxExtent;
		storeClassName:					elStoreClassName;
		storeMethodSelector:				elStoreMethodSelector;
		parts: theParts.

	aDefinedWindow reorderLayoutRectangles.

	^aDefinedWindow! !

!CMDefinedWindow class publicMethodsFor: 'navigation'!

metaPerspectives

	^super metaPerspectives, self metaPerspectivesWindow!

metaPerspectivesGeneral

	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Name' 'Kind' 'IsDirty' 'StoreMethodSelector' 'StoreClassName')))
			nlsApp: 'CMDF';
			nlsGroup: 'General_Perspectives';
			nlsItem: 'General';
			nlsTranslation: 'General';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'PersistIfDirty'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('IsDirty' 'DoPersistIfDirty')))
			nlsApp: 'CMDF';
			nlsGroup: 'General_Perspectives';
			nlsItem: 'PersistIfDirty';
			nlsTranslation: 'GrabarSiPendiente';
			yourself);
		yourself!

metaPerspectivesWindow
	^OrderedCollection new
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Specific'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Model' 'TitleDefaultTranslation' 'TitleNlsSymbol' 'TitleNlsGroup' 'NLS' 'NlsItem' 'StoreClassName' 'StoreMethodSelector')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'Specific';
			nlsTranslation: 'Especificos de Ventana';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Dimensions'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('OriginX' 'OriginY' 'MinExtentX' 'MinExtentY' 'PrefExtentX' 'PrefExtentY' 'MaxExtentX' 'MaxExtentY')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'Dimensions';
			nlsTranslation: 'Dimensiones';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Parts'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Parts')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'Parts';
			nlsTranslation: 'Partes';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Allowed'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Model' 'AllowMode' 'AllowExpression')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'Allowed';
			nlsTranslation: 'Disponibilidad';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Model'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('Model')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'Model';
			nlsTranslation: 'Modelo';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'AllowedRootTypes'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('AllowedRootTypes')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'AllowedRootTypes';
			nlsTranslation: 'DisponibleParaTiposRaiz';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'AllowedSubTypes'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('AllowedSubTypes')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'AllowedSubTypes';
			nlsTranslation: 'DisponibleParaSubTipos';
			yourself);
		addLast:  (((Smalltalk at: #METAPerspectiveSpec)  
			name: 'ApplicationTranslation'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsToSelect: #('NLSappName' 'DefaultApplicationTranslationStoreClassName'  'DefaultApplicationTranslationStoreMethodSelector')))
			nlsApp: 'CMDF';
			nlsGroup: 'DefinedWindow_Perspectives';
			nlsItem: 'ApplicationTranslation';
			nlsTranslation: 'AplicacionDeTraduccion';
			yourself);

		yourself!

metaSelectors

	^super metaSelectors , self metaSelectorsWindow!

metaSelectorsWindow

	"METAChildSpecAutoViewEditor openOn: CMDefinedWindow selector: #metaSelectorsWindow target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 18)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'NLSappName';
			basicSelector: #nlsAppName;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'NLSappName';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'NLSappName';
			nlsTranslation: 'NombreDeAplicacionDeTraduccion';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DefaultApplicationTranslationStoreClassName';
			basicSelector: #defaultApplicationTranslationStoreClassName;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DefaultApplicationTranslationStoreClassName';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'DefaultApplicationTranslationStoreClassName';
			nlsTranslation: 'NombreDeClaseDeGrabacionDeTraduccion';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DefaultApplicationTranslationStoreMethodSelector';
			basicSelector: #defaultApplicationTranslationStoreMethodSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DefaultApplicationTranslationStoreMethodSelector';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'DefaultApplicationTranslationStoreMethodSelector';
			nlsTranslation: 'SelectorDeGrabacionDeTraduccion';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'TitleDefaultTranslation';
			basicSelector: #titleDefaultTranslation;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TitleDefaultTranslation';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'TitleDefaultTranslation';
			nlsTranslation: 'TraduccionPorDefectoDelTitulo';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'TitleNlsSymbol';
			basicSelector: #titleNlsSymbol;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TitleNlsSymbol';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'TitleNlsSymbol';
			nlsTranslation: 'SimboloTraduccionDelTitulo';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'TitleNlsGroup';
			basicSelector: #titleNlsGroup;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TitleNlsGroup';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'TitleNlsGroup';
			nlsTranslation: 'GrupoTraduccionDelTitulo';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'OriginX';
			basicSelector: #originX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'OriginX';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'OriginX';
			nlsTranslation: 'Origen X';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'OriginY';
			basicSelector: #originY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'OriginY';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'OriginY';
			nlsTranslation: 'Origen Y';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MinExtentX';
			basicSelector: #minExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentX';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MinExtentX';
			nlsTranslation: 'Anchura Minima';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MinExtentY';
			basicSelector: #minExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentY';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MinExtentY';
			nlsTranslation: 'Altura Minima';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'PrefExtentX';
			basicSelector: #prefExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'PrefExtentX';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'PrefExtentX';
			nlsTranslation: 'Anchura Preferida';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'PrefExtentY';
			basicSelector: #prefExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'PrefExtentY';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'PrefExtentY';
			nlsTranslation: 'Altura Preferida';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MaxExtentX';
			basicSelector: #maxExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentX';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MaxExtentX';
			nlsTranslation: 'AnchuraMaxima';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'MaxExtentY';
			basicSelector: #maxExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentY';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MaxExtentY';
			nlsTranslation: 'Altura Maxima';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'StoreClassName';
			basicSelector: #storeClassName;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'StoreClassName';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'StoreClassName';
			nlsTranslation: 'Nombre Clase Almacenamiento';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'StoreMethodSelector';
			basicSelector: #storeMethodSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'StoreMethodSelector';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'StoreMethodSelector';
			nlsTranslation: 'Selector Almacenamiento';
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Parts';
			basicSelector: #parts;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Parts';
			displaySelector: #name;
			canShowInTree: true;
			componentsClassName: #CMDefinedPart;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'Parts';
			nlsTranslation: 'Partes';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'AllowMode';
			basicSelector: #allowMode;
			type: #Enum;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowMode';
			displaySelector: nil;
			canShowInTree: true;
			enumValuesString: 'all allAllowed onlySubTypes allowAllSubTypes byConstraint allowVirtualTypeMembers none';
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'AllowMode';
			nlsTranslation: 'ModoDisponibilidad';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'AllowExpression';
			basicSelector: #allowExpression;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowExpression';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'AllowExpression';
			nlsTranslation: 'ExpresionDisponibilidad';
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Model';
			basicSelector: #model;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Select;
			helpString: 'MetaInfo';
			displaySelector: nil;
			objectClassName: #CODEModule;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'Model';
			nlsTranslation: 'Modelo';
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'AllowedRootTypes';
			basicSelector: #allowedRootTypes;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowedRootTypes';
			displaySelector: #name;
			canShowInTree: true;
			componentsClassName: #CODEType;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'AllowedRootTypes';
			nlsTranslation: 'TiposRaizPermitidos';
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'AllowedSubTypes';
			basicSelector: #allowedSubTypes;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowedSubTypes';
			displaySelector: #name;
			canShowInTree: true;
			componentsClassName: #CODEType;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'AllowedSubTypes';
			nlsTranslation: 'SubTiposPermitidos';
			yourself);
		yourself!

xmetaSelectorsWindow

	"METAChildSpecAutoViewEditor openOn: self selector: #metaSelectorsWindow target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 12)
		add: (METATerminalChildSpec new
			name: 'TitleDefaultTranslation';
			basicSelector: #titleDefaultTranslation;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TitleDefaultTranslation';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'TitleDefaultTranslation';
			nlsTranslation: 'TraduccionPorDefectoDelTitulo';
			yourself);
		add: (METATerminalChildSpec new
			name: 'TitleNlsSymbol';
			basicSelector: #titleNlsSymbol;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TitleNlsSymbol';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'TitleNlsSymbol';
			nlsTranslation: 'SimboloDeTraduccion';
			yourself);
		add: (METATerminalChildSpec new
			name: 'TitleNlsGroup';
			basicSelector: #titleNlsGroup;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'TitleNlsGroup';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'TitleNlsGroup';
			nlsTranslation: 'GrupoDeTraduccion';
			yourself);
		add: (METATerminalChildSpec new
			name: 'OriginX';
			basicSelector: #originX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'OriginX';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'OriginX';
			nlsTranslation: 'OrigenX';
			yourself);
		add: (METATerminalChildSpec new
			name: 'OriginY';
			basicSelector: #originY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'OriginY';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'OriginY';
			nlsTranslation: 'OrigenY';
			yourself);
		add: (METATerminalChildSpec new
			name: 'MinExtentX';
			basicSelector: #minExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentX';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MinExtentX';
			nlsTranslation: 'AnchuraMinima';
			yourself);
		add: (METATerminalChildSpec new
			name: 'MinExtentY';
			basicSelector: #minExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MinExtentY';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MinExtentY';
			nlsTranslation: 'AlturaMinima';
			yourself);
		add: (METATerminalChildSpec new
			name: 'PrefExtentX';
			basicSelector: #prefExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'PrefExtentX';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'PrefExtentX';
			nlsTranslation: 'AnchuraPreferida';
			yourself);
		add: (METATerminalChildSpec new
			name: 'PrefExtentY';
			basicSelector: #prefExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'PrefExtentY';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'PrefExtentY';
			nlsTranslation: 'AlturaPreferida';
			yourself);
		add: (METATerminalChildSpec new
			name: 'MaxExtentX';
			basicSelector: #maxExtentX;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentX';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MaxExtentX';
			nlsTranslation: 'AnchuraMaxima';
			yourself);
		add: (METATerminalChildSpec new
			name: 'MaxExtentY';
			basicSelector: #maxExtentY;
			type: #Number;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'MaxExtentY';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'MaxExtentY';
			nlsTranslation: 'AlturaMaxima';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'StoreClassName';
			basicSelector: #storeClassName;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'StoreClassName';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'StoreClassName';
			nlsTranslation: 'ClaseDeAlmacenamiento';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'StoreMethodSelector';
			basicSelector: #storeMethodSelector;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'StoreMethodSelector';
			displaySelector: nil;
			canShowInTree: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'StoreMethodSelector';
			nlsTranslation: 'SelectorDeAlmacenamiento';
			yourself);
		add: (METAOrderedCollectionChildSpec new
			name: 'Parts';
			basicSelector: #parts;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Parts';
			displaySelector: #name;
			componentsClassName: #CMDefinedPart;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'Parts';
			nlsTranslation: 'Partes';
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'AllowMode';
			basicSelector: #allowMode;
			type: #Enum;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowMode';
			displaySelector: nil;
			canShowInTree: true;
			enumValuesString: 'all allAllowed onlySubTypes allowAllSubTypes byConstraint allowVirtualTypeMembers none';
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'AllowMode';
			nlsTranslation: 'ModoTiposPermitidos';
			yourself);
		add: (METATerminalChildSpec new
			name: 'AllowExpression';
			basicSelector: #allowExpression;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowExpression';
			displaySelector: nil;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedPartField_Selectors';
			nlsItem: 'AllowExpression';
			nlsTranslation: 'ConstriccionDisponibilidad';
			yourself);
		add: (METAOrderedCollectionChildSpec new
			name: 'AllowedRootTypes';
			basicSelector: #allowedRootTypes;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowedRootTypes';
			displaySelector: #name;
			componentsClassName: #CODEType;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			nlsApp: 'CMDP';
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'AllowedRootTypes';
			nlsTranslation: 'TiposRaizPermitidos';
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'AllowedSubTypes';
			basicSelector: #allowedSubTypes;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'AllowedSubTypes';
			displaySelector: #name;
			componentsClassName: #CODEType;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			nlsGroup: 'DefinedWindow_Selectors';
			nlsItem: 'AllowedSubTypes';
			nlsTranslation: 'SubTiposPermitidos';
			yourself);

		yourself! !

!CMDefinedWindow publicMethodsFor: 'accessing'!

allowExpression
	^allowExpression!

allowExpression: elValor

	| unValor unosStrings |
	unValor := (elValor isNil or: [ elValor isEmpty])
		ifTrue: [ nil] 
		ifFalse: [ 	
			unosStrings := elValor  asArrayOfSubstrings.
			unosStrings isEmpty
				ifTrue: [ nil]
				ifFalse: [ elValor]
		].
	
	unValor = allowExpression  ifTrue: [ ^self].

	allowExpression := unValor.
	self markDirty.
	self changed: #allowExpression!

allowMode
	^allowMode!

allowMode: elValor

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
	
	unValor = allowMode  ifTrue: [ ^self].

	allowMode := unValor.
	self markDirty.
	self changed: #allowMode!

comment
	^comment!

comment: theValue
	comment := theValue.
	self changed: #comment!

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

	self changed: #defaultApplicationTranslationStoreMethodSelector!

forzeAllowExpression: elValor
	allowExpression := elValor!

forzeAllowMode: elValor
	allowMode := elValor!

layoutRectangles
	^layoutRectangles!

layoutRectangles: theValue
	layoutRectangles := theValue.!

maxExtent
	^maxExtent!

maxExtent: thePoint
	maxExtent := thePoint.
	self changed: #maxExtent!

maxExtentX
	^self maxExtent isNil ifTrue: [ nil] ifFalse: [ self maxExtent x]!

maxExtentX: theValue
	theValue isNil ifTrue: [ ^self].

	maxExtent isNil 
		ifTrue: [ maxExtent := theValue @ 0]
		ifFalse: [ maxExtent x: theValue].

	self changed: #maxExtentX.
	self changed: #maxExtent!

maxExtentY
	^self maxExtent isNil ifTrue: [ nil] ifFalse: [ self maxExtent y]!

maxExtentY: theValue
	theValue isNil ifTrue: [ ^self].
	maxExtent isNil 
		ifTrue: [ maxExtent := 0 @ theValue ]
		ifFalse: [ maxExtent y: theValue].

	self changed: #maxExtentY.
	self changed: #maxExtent!

minExtent
	^minExtent!

minExtent: elPoint
	minExtent := elPoint.
	self changed: #minExtent!

minExtentX
	^self minExtent isNil ifTrue: [ nil] ifFalse: [ self minExtent x]!

minExtentX: theValue
	theValue isNil ifTrue: [ ^self].
	minExtent isNil 
		ifTrue: [ minExtent := theValue @ 0]
		ifFalse: [ minExtent x: theValue].

	self changed: #minExtentX.
	self changed: #minExtent!

minExtentY
	^self minExtent isNil ifTrue: [ nil] ifFalse: [ self minExtent y]!

minExtentY: theValue
	theValue isNil ifTrue: [ ^self].
	minExtent isNil 
		ifTrue: [ minExtent := 0 @ theValue ]
		ifFalse: [ minExtent y: theValue].

	self changed: #minExtentY.
	self changed: #minExtent!

model
	^model!

model: theModel
	model := theModel.
	self changed: #model!

nlsAppName
	^nlsAppName!

nlsAppName: elAppName

	nlsAppName := elAppName.
	self changed: #nlsAppName!

origin
	^origin!

origin: elPoint
	origin := elPoint.
	self changed: #origin!

originX
	^self origin isNil ifTrue: [ nil] ifFalse: [ self origin x]!

originX: theValue
	theValue isNil ifTrue: [ ^self].
	origin isNil 
		ifTrue: [ origin := theValue @ 0]
		ifFalse: [ origin x: theValue].

	self changed: #originX.
	self changed: #origin!

originY
	^self origin isNil ifTrue: [ nil] ifFalse: [ self origin y]!

originY: theValue
	theValue isNil ifTrue: [ ^self].
	origin isNil 
		ifTrue: [ origin := 0 @ theValue ]
		ifFalse: [ origin y: theValue].

	self changed: #originY.
	self changed: #origin!

parts
	"Generated by ISF/AD. Do not modify"
	^self partsPrivate copy!

parts: elParts

	parts := elParts.
	parts isNil ifFalse: [ 
		parts do: [:aPart | aPart containerPrivate: self]]!

partsPrivate: elParts

	parts := elParts.!

prefExtent
	^prefExtent!

prefExtent: elPoint
	prefExtent := elPoint.
	self changed: #prefExtent!

prefExtentX
	^self prefExtent isNil ifTrue: [ nil] ifFalse: [ self prefExtent x]!

prefExtentX: theValue
	theValue isNil ifTrue: [ ^self].
	prefExtent isNil 
		ifTrue: [ prefExtent := theValue @ 0]
		ifFalse: [ prefExtent x: theValue].

	self changed: #prefExtentX.
	self changed: #prefExtent!

prefExtentY
	^self prefExtent isNil ifTrue: [ nil] ifFalse: [ self prefExtent y]!

prefExtentY: theValue
	theValue isNil ifTrue: [ ^self].
	prefExtent isNil 
		ifTrue: [ prefExtent := 0 @ theValue ]
		ifFalse: [ prefExtent y: theValue].

	self changed: #prefExtentY.
	self changed: #prefExtent!

storeClassName
	^storeClassName!

storeClassName: elValor
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
	
	unValor = storeClassName  ifTrue: [ ^self].

	storeClassName := unValor.
	self markDirty.

	self changed: #storeClassName!

storeMethodSelector
	^storeMethodSelector!

storeMethodSelector: elValor
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
	
	unValor = storeMethodSelector  ifTrue: [ ^self].

	storeMethodSelector := unValor.
	self markDirty.

	self changed: #storeMethodSelector!

timestamp
	^timestamp!

timestamp: theValue
	timestamp := theValue.
	self changed: #timestamp!

titleDefaultTranslation
	^titleDefaultTranslation!

titleDefaultTranslation: elTitleDefaultTranslation

	titleDefaultTranslation := elTitleDefaultTranslation.
	self changed: #titleDefaultTranslation!

titleNlsGroup
	^titleNlsGroup
"*VIPVersion 22-6-97 | 7:34:58 pm 'ACV'*"!

titleNlsGroup: elTitleNlsGroup

	titleNlsGroup := elTitleNlsGroup.
	self changed: #titleNlsGroup!

titleNlsSymbol
	^titleNlsSymbol
"*VIPVersion 22-6-97 | 7:34:58 pm 'ACV'*"!

titleNlsSymbol: elTitleNlsSymbol

	titleNlsSymbol := elTitleNlsSymbol.
	self changed: #titleNlsSymbol! !

!CMDefinedWindow publicMethodsFor: 'association initialize-release'!

allowedRootTypesRelease
	"Generated by ISF/AD. Do not modify"
	self allowedRootTypes do: [:each | self allowedRootTypesRemove: each]!

allowedSubTypesRelease
	"Generated by ISF/AD. Do not modify"
	self allowedSubTypes do: [:each | self allowedSubTypesRemove: each]!

initAllowedRootTypes
	"Generated by ISF/AD. Do not modify"
	allowedRootTypes := OrderedCollection new.!

initAllowedSubTypes
	"Generated by ISF/AD. Do not modify"
	allowedSubTypes := OrderedCollection new.! !

!CMDefinedWindow publicMethodsFor: 'associations accessing'!

allowedRootTypes
	"Generated by ISF/AD. Do not modify"
	^self allowedRootTypesPrivate copy!

allowedRootTypesAsArray
	"Generated by ISF/AD. Do not modify"
	^self allowedRootTypes asArray!

allowedSubTypes
	"Generated by ISF/AD. Do not modify"
	^self allowedSubTypesPrivate copy!

allowedSubTypesAsArray
	"Generated by ISF/AD. Do not modify"
	^self allowedSubTypes asArray! !

!CMDefinedWindow publicMethodsFor: 'associations modifying'!

allowedRootTypesAdd: aValue
	"Generated by ISF/AD. Do not modify"
	(self checkAllowedRootTypesAdd: aValue) ifFalse: [^aValue].
	(self allowedRootTypesIncludes: aValue) ifTrue: [^aValue].
	self allowedRootTypesPrivateAdd: aValue.
	self changed: #allowedRootTypes.
	^aValue!

allowedRootTypesRemove: aValue
	"Generated by ISF/AD. Do not modify"
	(self checkAllowedRootTypesRemove: aValue) ifFalse: [^aValue].
	self allowedRootTypesPrivate remove: aValue ifAbsent: [^aValue].
	self changed: #allowedRootTypes.
	^aValue!

allowedSubTypesAdd: aValue
	"Generated by ISF/AD. Do not modify"
	(self checkAllowedSubTypesAdd: aValue) ifFalse: [^aValue].
	(self allowedSubTypesIncludes: aValue) ifTrue: [^aValue].
	self allowedSubTypesPrivateAdd: aValue.
	self changed: #allowedSubTypes.
	^aValue!

allowedSubTypesRemove: aValue
	"Generated by ISF/AD. Do not modify"
	(self checkAllowedSubTypesRemove: aValue) ifFalse: [^aValue].
	self allowedSubTypesPrivate remove: aValue ifAbsent: [^aValue].
	self changed: #allowedSubTypes.
	^aValue! !

!CMDefinedWindow publicMethodsFor: 'associations private'!

allowedRootTypesPrivate
	"Generated by ISF/AD. Do not modify"
	allowedRootTypes isNil
		ifTrue: [self initAllowedRootTypes].
	^allowedRootTypes!

allowedRootTypesPrivateAdd: aValue
	"Generated by ISF/AD. Do not modify"
	self allowedRootTypesPrivate add: aValue.
	self markDirty.
	self changed: #allowedRootTypes.
	^aValue!

allowedRootTypesPrivateRemove: aValue
	"Generated by ISF/AD. Do not modify"
	self allowedRootTypesPrivate remove: aValue.
	self markDirty.
	self changed: #allowedRootTypes.
	^aValue!

allowedSubTypesPrivate
	"Generated by ISF/AD. Do not modify"
	allowedSubTypes isNil
		ifTrue: [self initAllowedSubTypes].
	^allowedSubTypes!

allowedSubTypesPrivateAdd: aValue
	"Generated by ISF/AD. Do not modify"
	self allowedSubTypesPrivate add: aValue.
	self markDirty.
	self changed: #allowedSubTypes.
	^aValue!

allowedSubTypesPrivateRemove: aValue
	"Generated by ISF/AD. Do not modify"
	self allowedSubTypesPrivate remove: aValue.
	self markDirty.
	self changed: #allowedSubTypes.
	^aValue!

forzeAllowedRootTypes: theAllowedRootTypes
	(theAllowedRootTypes isNil or: [ theAllowedRootTypes isEmpty]) ifTrue: [ ^self].
	self allowedRootTypesPrivate addAll: theAllowedRootTypes.!

forzeAllowedRootTypesRefsTmpValues: theValue
	allowedRootTypesRefsTmpValues := theValue!

forzeAllowedSubTypes: theAllowedSubTypes
	(theAllowedSubTypes isNil or: [ theAllowedSubTypes isEmpty]) ifTrue: [ ^self].
	self allowedSubTypesPrivate addAll: theAllowedSubTypes.!

forzeAllowedSubTypesRefsTmpValues: theValue
	allowedSubTypesRefsTmpValues := theValue!

forzeModel: theModel
	self model: theModel.!

forzeModelRefTmpValues: theValue
	modelRefTmpValues := theValue! !

!CMDefinedWindow publicMethodsFor: 'associations testing'!

allowedRootTypesIncludes: aValue
	"Generated by ISF/AD. Do not modify"
	^allowedRootTypes isNil
		ifTrue: [false]
		ifFalse: [self allowedRootTypes includes: aValue]!

allowedRootTypesSize
	"Generated by ISF/AD. Do not modify"
	^allowedRootTypes isNil
		ifTrue: [0]
		 ifFalse: [allowedRootTypes size]!

allowedSubTypesIncludes: aValue
	"Generated by ISF/AD. Do not modify"
	^allowedSubTypes isNil
		ifTrue: [false]
		ifFalse: [self allowedSubTypes includes: aValue]!

allowedSubTypesSize
	"Generated by ISF/AD. Do not modify"
	^allowedSubTypes isNil
		ifTrue: [0]
		 ifFalse: [allowedSubTypes size]! !

!CMDefinedWindow publicMethodsFor: 'comparing'!

compareParts: theOther

	| anAddedCatalog aRemovedCatalog |
	theOther isNil ifTrue: [ ^self].

	anAddedCatalog := self class new.
	aRemovedCatalog := self class new.


	self partsPrivate do: [:onePart | |  otherPart |
		otherPart := theOther partNamed: onePart name.
		otherPart isNil 
			ifTrue: [  aRemovedCatalog partsAdd: onePart copyParts]
			ifFalse: [ 
				onePart compareParts: otherPart 
					added: anAddedCatalog  removed: aRemovedCatalog.
			]
	].

	theOther partsPrivate do: [:onePart | |  otherPart |
		otherPart := self partNamed: onePart name.
		otherPart isNil 
			ifTrue: [ anAddedCatalog partsAdd: onePart copyParts]
	].


	anAddedCatalog partsSize < 1 
		ifTrue: [ anAddedCatalog := nil ]
		ifFalse: [ 
			anAddedCatalog name: self name. 
			anAddedCatalog sourceFileName: 'added' copy].

	aRemovedCatalog partsSize < 1 
		ifTrue: [ aRemovedCatalog := nil]
		ifFalse: [ 
			aRemovedCatalog name: self name.
			aRemovedCatalog sourceFileName: 'removed' copy].

	^Array with: anAddedCatalog with: aRemovedCatalog! !

!CMDefinedWindow publicMethodsFor: 'copying'!

copyParts
	| aCopy  |
	aCopy := self class new.
	aCopy name: self name copy.
	aCopy sourceFileName: self sourceFileName  copy.

	self partsPrivate do: [:aPart |
		aCopy partsAdd: aPart copyParts].

	^aCopy! !

!CMDefinedWindow publicMethodsFor: 'derived accessing'!

allowedTypesForSubPart

	| aModel anAllowExpression someTypes |
	aModel := self model.
	aModel isNil ifTrue: [ ^nil].

	self allowMode = self class allowModeNoneSymbol ifTrue: [ ^nil].
	self allowMode = self class allowModeAllSymbol ifTrue: [ ^aModel allTypes].
	self allowMode = self class allowModeByConstraintSymbol ifTrue: [ 
		anAllowExpression := self allowExpression.
		(anAllowExpression isNil or: [ anAllowExpression asArrayOfSubstrings isEmpty]) ifTrue: [ ^aModel allTypes].
		someTypes := aModel allTypes select: [:aType | aType canEvaluateOnInstances: anAllowExpression].
		^someTypes
	].

	someTypes := IdentitySet new: 13.
	self allowMode = self class allowModeAllAllowedSymbol ifTrue: [ 
		someTypes 
			addAll: (self allowedRootTypes select: [:aT | aT isAbstract not]);
			addAll: (self allowedSubTypes select: [:aT | aT isAbstract not]).
		self allowedRootTypesPrivate do: [:aType | someTypes addAll: (aType allSubTypes select: [:aT | aT isAbstract not])].
		self allowedSubTypes do: [:aType | someTypes addAll: (aType allSubTypes select: [:aT | aT isAbstract not])].
		^someTypes asOrderedCollection
	].

	self allowMode = self class allowModeAllowAllSubTypesSymbol ifTrue: [ 
		someTypes addAll: (self allowedSubTypes select: [:aT | aT isAbstract not]).
		self allowedRootTypesPrivate do: [:aType | someTypes addAll: (aType allSubTypes select: [:aT | aT isAbstract not])].
		self allowedSubTypes do: [:aType | someTypes addAll: (aType allSubTypes select: [:aT | aT isAbstract not])].
		^someTypes asOrderedCollection
	].

	self allowMode = self class allowModeOnlySubTypesSymbol ifTrue: [ 
		self allowedRootTypesPrivate do: [:aType | someTypes addAll: (aType allSubTypes select: [:aT | aT isAbstract not])].
		^someTypes asOrderedCollection
	].

	self allowMode = self class allowModeAllowVirtualTypeMembersSymbol ifTrue: [ 
		someTypes 
			addAll: (self allowedRootTypes select: [:aT | aT isAbstract not]);
			addAll: (self allowedSubTypes select: [:aT | aT isAbstract not]).
		self allowedRootTypesPrivate do: [:aType | someTypes addAll: (aType allSubTypes select: [:aT | aT isAbstract not])].
		self allowedSubTypes do: [:aType | someTypes addAll: (aType allSubTypes select: [:aT | aT isAbstract not])].
		someTypes 
			addAll: (self allowedRootTypes memberInstanceTypes select: [:aT | aT isAbstract not]);
			addAll: (self allowedSubTypes memberInstanceTypes select: [:aT | aT isAbstract not]).
		self allowedRootTypesPrivate do: [:aType | someTypes addAll: (aType allSubTypes memberInstanceTypes select: [:aT | aT isAbstract not])].
		self allowedSubTypes do: [:aType | someTypes addAll: (aType allSubTypes memberInstanceTypes select: [:aT | aT isAbstract not])].
		^someTypes asOrderedCollection
	].

	^nil!

emptyParts
	^self partsPrivate select: [:anPart |		
		anPart groupPartsSize < 1]!

subGroups
	^self partsPrivate select: [:aPart | aPart isGroup]! !

!CMDefinedWindow publicMethodsFor: 'dirty'!

cleanDirtyMark
	super cleanDirtyMark.
	self parts do: [:aPart | aPart cleanDirtyMark]!

isDirty
	^dirty == true!

markDirty
	dirty == true ifFalse: [ 
		dirty := true.
		self changed: #dirty
	]!

markDirtyContainer
	self markDirty!

persistIfDirty

	| aNLSSolver |
	CMDefinedPartsInstaller persistCMDefinedWindow: self  filter: #source.
	self cleanDirtyMark.

	aNLSSolver := self nlsSolver.
	aNLSSolver isNil ifFalse: [ aNLSSolver persistIfDirty].! !

!CMDefinedWindow publicMethodsFor: 'history'!

snapshot
	
	| aSnapshot |
	aSnapshot := self copyParts.
	aSnapshot addToCatalogsHistory.
	^aSnapshot! !

!CMDefinedWindow publicMethodsFor: 'initialize-release'!

initialize
	super initialize.

	titleDefaultTranslation := nil.
	titleNlsSymbol := nil.
	titleNlsGroup := nil.
	origin := nil.
	minExtent := nil.
	prefExtent := nil.
	maxExtent := nil.
	parts := nil.
	origin := nil.
	storeClassName := nil.
	storeMethodSelector := nil.
	
	self allowedRootTypesRelease.
	self allowedSubTypesRelease.

	layoutRectangles := Array new.!

release
	"Generated by ISF/AD. Do not modify"

	titleDefaultTranslation := nil.
	titleNlsSymbol := nil.
	titleNlsGroup := nil.
	origin := nil.
	minExtent := nil.
	prefExtent := nil.
	maxExtent := nil.
	storeClassName := nil.
	storeMethodSelector := nil.

	self partsRelease.
	parts := nil.

	super release! !

!CMDefinedWindow publicMethodsFor: 'layout'!

initializeLayoutsFromEditor: theEditor
	| someParts aLayoutGroup someLayouts someReorganizedParts |
	theEditor isNil  ifTrue: [ ^self].

	someParts := self parts.
	(someParts isNil or: [ someParts isEmpty]) ifTrue: [ ^self].

	self testIfAllSubPartsHaveSameLayoutGroups  ifFalse: [ 
 Transcript show: self kind , ' ',  self name , ' can not initializeLayoutsFromEditor because subParts have more than one LayoutGroup'; cr.
		someParts do: [:aPart | aPart isGroup ifTrue: [ aPart  initializeLayoutsFromEditor: theEditor]].
		^self
	].
	
	aLayoutGroup := someParts first layoutGroup.
	someLayouts := theEditor defaultLayoutsFor: aLayoutGroup.
	(someLayouts isNil or: [someLayouts isEmpty]) ifTrue: [ ^self].

	layoutRectangles := someLayouts.

	someReorganizedParts := OrderedCollection new: someParts size * 2.

	someLayouts do: [:aLayout |   | aLayoutSymbol aPart |
		aLayoutSymbol := self class layoutSymbolFromLCA: aLayout.
		aLayoutSymbol isNil ifFalse: [ 
			aLayoutSymbol = #sep
				ifTrue: [
					aPart := CMDefinedPartSep named: 'sep '.
					aPart layoutSymbol: aLayoutSymbol.
					aPart layoutGroup: aLayoutGroup.	
					aPart initLayoutRectangleFromIdentical: aLayout.
					aPart containerPrivate: self.
					someReorganizedParts add: aPart			
				]
				ifFalse: [ 
					aPart := self partWithLayoutSymbol: aLayoutSymbol.
					aPart isNil 
						ifFalse:  [ 
							aPart initLayoutRectangleFromIdentical: aLayout.
							someReorganizedParts add: aPart.
							aPart isMultiList ifTrue: [ aPart initializeLayoutsFromEditor: theEditor]
						]
						ifTrue: [ 
							aPart := CMDefinedPartAnonymous named: 'anon ', aLayoutSymbol.
							aPart layoutSymbol: aLayoutSymbol.
							aPart layoutGroup: aLayoutGroup.
							aPart initLayoutRectangleFromIdentical: aLayout.
							aPart containerPrivate: self.
							someReorganizedParts add: aPart
						]
				]
		]
	].
	someParts do: [:aPart |
		((aPart isKindOf: CMDefinedPartSep) or: [ aPart isKindOf: CMDefinedPartAnonymous]) ifFalse: [ 
			(someReorganizedParts includes: aPart) ifFalse: [ 
				someReorganizedParts add: aPart
			]
		]
	].

	self partsPrivate: someReorganizedParts.

	someParts do: [:aPart | aPart isGroup ifTrue: [ aPart  initializeLayoutsFromEditor: theEditor]].!

reorderLayoutRectangles
	| someParts someLayoutRectangles  |
	someParts := self parts.
	(someParts isNil or: [ someParts isEmpty]) ifTrue: [ ^self].

	someLayoutRectangles := OrderedCollection new: self partsSize.
	self partsPrivate do: [:aPart | | aLayoutRectangle |
		aPart hasLayoutRectangle ifTrue: [ 
			aLayoutRectangle := aPart layoutRectangle.
			aLayoutRectangle isNil ifFalse: [ someLayoutRectangles add: aLayoutRectangle]]].

	layoutRectangles := someLayoutRectangles! !

!CMDefinedWindow publicMethodsFor: 'nls'!

nls
	
	| aResolverItem aResult |

	aResolverItem := self nlsResolverItem.
	aResolverItem isNil ifTrue: [ ^self titleNlsSymbol].
	aResult := aResolverItem translation.
	aResult isNil ifTrue: [ ^self titleNlsSymbol].

	^aResult!

nls: theNewTranslation
	
	| aResolverItem aResult |

	theNewTranslation isNil ifTrue: [ ^self].

	aResolverItem := self nlsResolverItem.
	aResolverItem isNil ifTrue: [ ^self].

	aResult := aResolverItem translation.
	aResult isNil ifFalse: [ aResult = theNewTranslation ifTrue: [ ^self ]].

	aResolverItem translation: theNewTranslation.!

nlsResolverItem
	| aTopPart anEditorClassName aGroup anItem aResult |

	aTopPart := self topPart.
	aTopPart isFromEditorClass ifFalse: [ ^nil].

	anEditorClassName := aTopPart editorClassName.
	anEditorClassName isNil ifTrue: [ ^nil].
	aGroup := self titleNlsGroup.
	aGroup isNil ifTrue: [ ^nil].
	anItem := self titleNlsSymbol.
	anItem isNil ifTrue: [ ^nil].

	aResult := TranslationsHome  nlsResolverItemApp: anEditorClassName asString group: aGroup item: anItem.
	^aResult!

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

	aResult isNil  ifTrue: [  
		aResult := Object messageNotUnderstoodSignal 
			handle: [:anEx | anEx returnWith: nil]
			do:  [ aClass perform: aMethodSelector]
	].
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

!CMDefinedWindow publicMethodsFor: 'part initialize-release'!

initParts
	"Generated by ISF/AD. Do not modify"
	parts := OrderedCollection new.!

partsRelease
	"Generated by ISF/AD. Do not modify"
	self parts do: [:each | self partsRemove: each]! !

!CMDefinedWindow publicMethodsFor: 'parts accessing'!

partNamed: theName
	"Generated by ISF/AD. Do not modify"
	theName isNil ifTrue: [ ^nil].
	^self partsPrivate detect: [:anAT | anAT name = theName] ifNone: [nil]!

partsAsArray
	"Generated by ISF/AD. Do not modify"
	^self parts asArray!

partWithLayoutSymbol: theLayoutSymbol
	theLayoutSymbol isNil ifTrue: [ ^nil].

	^self parts detect: [:aPart | theLayoutSymbol = aPart layoutSymbol] ifNone: [ nil]! !

!CMDefinedWindow publicMethodsFor: 'parts creation'!

partsLinkCreate
	
	| aMenu aFactory aPart anExistingNLSPart anExistingLayoutPart |
	aMenu := self class menuCMDefinedPartKinds.
	aMenu isNil ifTrue: [ ^nil].
	
	aFactory := aMenu startUp.
	aFactory isNil ifTrue: [ ^nil].
	aFactory == 0 ifTrue: [ ^nil].



	aPart := aFactory named: 'new ', aFactory kind.
	self partsAdd: aPart.
	self reorderLayoutRectangles.

	aPart isNLS ifTrue: [ 
		anExistingNLSPart := self partsPrivate detect: [:aDP | 
			aDP isNLS and: [ aDP nlsGroup isNil not and: [ aDP nlsGroup isEmpty not]]] ifNone: [ nil].
		anExistingNLSPart isNil ifFalse: [ aPart nlsGroup: anExistingNLSPart nlsGroup]].
	
	aPart hasLayoutRectangle ifTrue: [ 
		anExistingLayoutPart := self partsPrivate detect: [:aDP | 
			aDP hasLayoutRectangle and: [ aDP layoutGroup isNil not and: [ aDP layoutGroup isEmpty not]]] ifNone: [ nil].
		anExistingLayoutPart isNil ifFalse: [ 
			aPart layoutSymbol: (anExistingLayoutPart layoutSymbol , self partsSize printString) asSymbol.
			aPart layoutGroup: anExistingLayoutPart layoutGroup]].

	self changed: #parts.	

	^aPart! !

!CMDefinedWindow publicMethodsFor: 'parts modifying'!

partsAdd: aValue
	"Generated by ISF/AD. Do not modify"
	(self partsIncludes: aValue) ifTrue: [^self partsMoveBottom: aValue].
	(self partsPrivateAdd: aValue) containerPrivate: self.
	self changed: #parts.
	^aValue!

partsMoveBottom: aValue
	"Generated by ISF/AD. Do not modify"
	| aR |
	(self partsIncludes: aValue) ifFalse: [^aValue].
	(self parts indexOf: aValue) = self partsSize ifTrue: [^aValue].
	self partsPrivate remove: aValue.
	aR := self partsPrivateAdd: aValue.
	self reorderLayoutRectangles.
	self changed: #parts.
	^aR!

partsMoveDown: aValue
	"Generated by ISF/AD. Do not modify"
	| index aR |
	(self partsIncludes: aValue) ifFalse: [^aValue].
	(index := self parts indexOf: aValue) = self partsSize ifTrue: [^aValue].
	index = (self partsSize -1)
		ifTrue:
			[self partsPrivate remove: aValue.
			aR := self partsPrivateAdd: aValue.
			self reorderLayoutRectangles.
			^aR].
	aR := self partsPrivateMove: aValue beforeIndex: index + 2.
	self reorderLayoutRectangles.
	self changed: #parts.

	^aR!

partsMoveTop: aValue
	"Generated by ISF/AD. Do not modify"
	(self partsIncludes: aValue) ifFalse: [^aValue].
	(self parts indexOf: aValue) = 1 ifTrue: [^aValue].
	self partsPrivateMove: aValue beforeIndex: 1.
	self reorderLayoutRectangles.
	self changed: #parts.!

partsMoveUp: aValue
	"Generated by ISF/AD. Do not modify"
	| index aR |
	(self partsIncludes: aValue) ifFalse: [^aValue].
	(index := self parts indexOf: aValue) = 1 ifTrue: [^aValue].
	aR := self partsPrivateMove: aValue beforeIndex: index - 1.
	self reorderLayoutRectangles.
	self changed: #parts.

	^aR!

partsRemove: aValue
	"Generated by ISF/AD. Do not modify"
	(self partsPrivate remove: aValue ifAbsent: [^aValue]) containerPrivate: nil.
	self changed: #parts.
	^aValue! !

!CMDefinedWindow publicMethodsFor: 'parts private'!

partsPrivate
	"Generated by ISF/AD. Do not modify"
	parts isNil
		ifTrue: [self initParts].
	^parts!

partsPrivateAdd: aValue
	"Generated by ISF/AD. Do not modify"
	self partsPrivate add: aValue.
	self markDirty.

	self changed: #parts.
	^aValue!

partsPrivateMove: aValue beforeIndex: anIndex
	"Generated by ISF/AD. Do not modify"
	| obj |
	obj := self parts at: anIndex.
	self partsPrivate remove: aValue.
	self partsPrivate add: aValue before: obj.
	self markDirty.

	self changed: #parts.
	^aValue!

partsPrivateRemove: aValue
	"Generated by ISF/AD. Do not modify"
	self partsPrivate remove: aValue.
	self markDirty.

	self changed: #parts.
	^aValue!

subPartsPrivateAdd: aValue
	^self partsPrivateAdd: aValue!

subPartsPrivateRemove: aValue
	^self partsPrivateRemove: aValue! !

!CMDefinedWindow publicMethodsFor: 'parts testing'!

partsIncludes: aValue
	"Generated by ISF/AD. Do not modify"
	^parts isNil
		ifTrue: [false]
		ifFalse: [self parts includes: aValue]!

partsSize
	"Generated by ISF/AD. Do not modify"
	^parts isNil
		ifTrue: [0]
		 ifFalse: [parts size]! !

!CMDefinedWindow publicMethodsFor: 'persistence-code'!

allowedRootTypesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |

	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.


	(self allowedRootTypesPrivate isNil not and: [ self allowedRootTypesPrivate isEmpty not]) 
		ifFalse: [  theStream isNil ifFalse: [  theStream nextPutAll: anIS;  nextPutAll: (self pcForV: nil) ; cr]]
		ifTrue:  [ 
			theStream isNil ifFalse: [ 
				theStream 
		     		nextPutAll: anIS;  nextPutAll: '('; nextPutAll: (self pcForV: self  class allowedRootTypesPersistenceSymbol); cr
			].
			self allowedRootTypesPrivate do:  [:aType |
				aType asReferenceAsCodeStringOn: theStream indent: anIS , self indentStringForPersistenceAsCode
			].
			theStream isNil ifFalse: [  theStream  nextPutAll: anIS; nextPutAll: ' )' ; cr].
		].!

allowedSubTypesPersistenceAsCodeStringOn: theStream indent: theIS

	| anIS aSep |

	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.


	(self allowedSubTypesPrivate isNil not and: [ self allowedSubTypesPrivate isEmpty not]) 
		ifFalse: [  theStream isNil ifFalse: [  theStream nextPutAll: anIS;  nextPutAll: (self pcForV: nil) ; cr]]
		ifTrue:  [ 
			theStream isNil ifFalse: [ 
				theStream 
		     		nextPutAll: anIS;  nextPutAll: '('; nextPutAll: (self pcForV: self  class allowedSubTypesPersistenceSymbol); cr
			].
			self allowedSubTypesPrivate do:  [:aType |
				aType asReferenceAsCodeStringOn: theStream indent: anIS , self indentStringForPersistenceAsCode
			].
			theStream isNil ifFalse: [  theStream  nextPutAll: anIS; nextPutAll: ' )' ; cr].
		].!

bindToReferencedModel
	
	| aModel |

	(modelRefTmpValues isNil or: [ modelRefTmpValues isEmpty]) ifTrue: [ ^nil].
	
	aModel := CODEElement resolveReferencedModelFromPersistenceAsCodeOrCurrent:  modelRefTmpValues.

	aModel isNil ifFalse: [  self model: aModel] .

	modelRefTmpValues := nil!

firstPersistenceIndexDefinedWindow
	^self numberPersistenceEntriesDefinedPart + 1!

initAllowedRootTypesFromValues: theValues 
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].

	theValues first = self class allowedRootTypesPersistenceSymbol ifFalse: [ ^nil].

	allowedRootTypesRefsTmpValues := theValues.!

initAllowedRootTypesRefsTmpValuesFromModel 
	(self allowedRootTypesPrivate isNil or: [ self allowedRootTypesPrivate isEmpty]) ifTrue: [ ^self].
	
	allowedRootTypesRefsTmpValues := self allowedRootTypesPrivate collect:  [:aType | aType asReferenceArray ].!

initAllowedSubTypesFromValues: theValues 
	
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].

	theValues first = self class allowedSubTypesPersistenceSymbol ifFalse: [ ^nil].

	allowedSubTypesRefsTmpValues := theValues.!

initAllowedSubTypesRefsTmpValuesFromModel 
	(self allowedSubTypesPrivate isNil or: [ self allowedSubTypesPrivate isEmpty]) ifTrue: [ ^self].
	
	allowedSubTypesRefsTmpValues := self allowedSubTypesPrivate collect:  [:aType | aType asReferenceArray ].!

initFromValues: theValues
	
	| someLayoutRectangles aFPI |
	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^self].

	super initFromValues: theValues.
	aFPI := self firstPersistenceIndexDefinedWindow.

	self titleDefaultTranslation: 		(theValues size < (aFPI + 0)  ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 0)]). 
	self titleNlsSymbol: 					(theValues size < (aFPI + 1) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 1)]). 
	self titleNlsGroup: 					(theValues size < (aFPI + 2) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 2)]).
	self originX: 							(theValues size < (aFPI + 3) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 3)]). 
	self originY: 							(theValues size < (aFPI + 4) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 4)]). 
	self minExtentX: 					(theValues size < (aFPI + 5) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 5)]). 
	self minExtentY: 					(theValues size < (aFPI + 6) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 6)]). 
	self prefExtentX: 					(theValues size < (aFPI + 7) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 7)]). 
	self prefExtentY: 					(theValues size < (aFPI + 8) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 8)]). 
	self maxExtentX: 					(theValues size < (aFPI + 9) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 9)]). 
	self maxExtentY: 					(theValues size < (aFPI + 10) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 10)]). 
	self storeClassName: 				(theValues size < (aFPI + 11) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 11)]). 
	self storeMethodSelector: 			(theValues size < (aFPI + 12) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 12)]). 
	self forzeAllowMode:	 			(theValues size < (aFPI + 13) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 13)]). 
	self forzeAllowExpression:			(theValues size < (aFPI + 14) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 14)]). 
	self nlsAppName:					(theValues size < (aFPI + 15) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 15)]). 
	self defaultApplicationTranslationStoreClassName:		(theValues size < (aFPI + 16) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 16)]). 
	self defaultApplicationTranslationStoreMethodSelector:	(theValues size < (aFPI + 17) ifTrue: [nil] ifFalse: [ theValues at: (aFPI + 17)]). 

	theValues size < (aFPI + 18) ifFalse: [ self initReferencedModelFromValues: (theValues at: (aFPI + 18))].
	theValues size < (aFPI + 19) ifFalse: [ self initAllowedRootTypesFromValues: (theValues at: (aFPI + 19))].
	theValues size < (aFPI + 20) ifFalse: [ self initAllowedSubTypesFromValues: (theValues at: (aFPI + 20))].

	self parts: (theValues size < (aFPI + 21) ifTrue: [nil] ifFalse: [
		CMDefinedPart newPartsCollectionFromPersistenceAsCode: ( theValues at: (aFPI + 21))]).

	someLayoutRectangles := OrderedCollection new: self partsSize.
	self partsPrivate do: [:aPart | | aLayout | 
		aPart hasLayoutRectangle ifTrue: [ 
			aLayout := aPart layoutRectangle.
			aLayout isNil ifFalse: [ someLayoutRectangles add: aLayout]
		]
	].

	layoutRectangles := someLayoutRectangles asArray!

initModelRefTmpValuesFromModel 
	self model isNil ifTrue: [ ^self].

	modelRefTmpValues := self model persistenceRefToMethodArray!

initReferencedModelFromValues: theValues

	(theValues isNil or: [ theValues isEmpty]) ifTrue: [ ^nil].

	theValues first = CODEElement refToModelMethodKind ifFalse: [ ^nil].

	modelRefTmpValues := theValues.!

localValuesPersistenceAsCodeStringChunks: theCollection on: theStream filter: theFilter indent: theIS doSubChunks: theDoSubChunks

	| anIS aSep aModel |
	
	anIS := theIS , self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.

	theStream isNil ifFalse: [ 
		super localValuesPersistenceAsCodeStringOn: theStream indent: theIS.
		theStream 
			nextPutAll: (self pcForV: self titleDefaultTranslation); cr;
		     nextPutAll: anIS;  nextPutAll: (self pcForV: self titleNlsSymbol); nextPutAll: aSep;  
			nextPutAll: (self pcForV: self titleNlsGroup); cr;
			nextPutAll: anIS;  nextPutAll: (self pcForV: self originX);  nextPutAll: aSep;  
			nextPutAll: (self pcForV: self originY);  nextPutAll: aSep;  
			nextPutAll: (self pcForV: self minExtentX);  nextPutAll: aSep;  
			nextPutAll: (self pcForV: self minExtentY);  nextPutAll: aSep;  
			nextPutAll: (self pcForV: self prefExtentX);  nextPutAll: aSep;  
			nextPutAll: (self pcForV: self prefExtentY);  nextPutAll: aSep;  
			nextPutAll: (self pcForV: self maxExtentX);  nextPutAll: aSep;  
			nextPutAll: (self pcForV: self maxExtentY);  cr;
		     nextPutAll: anIS;  nextPutAll: (self pcForV:  self storeClassName );  
			nextPutAll: aSep; nextPutAll: (self pcForV:  self storeMethodSelector ); cr;
		     nextPutAll: anIS;  nextPutAll: (self pcForV:  self allowMode );  cr;
		     nextPutAll: anIS;  nextPutAll: (self pcForV:  self allowExpression );  cr;
			nextPutAll: anIS; nextPutAll: (self pcForV:  self nlsAppName ); cr;
			nextPutAll: anIS; nextPutAll: (self pcForV:  self defaultApplicationTranslationStoreClassName ); nextPutAll: aSep;  
			nextPutAll: anIS; nextPutAll: (self pcForV:  self defaultApplicationTranslationStoreMethodSelector ); cr.
		
		aModel := self model.
		aModel isNil  
			ifTrue: [ theStream nextPutAll: anIS; nextPutAll: (self pcForV: nil)] 
			ifFalse: [ aModel persistenceRefToMethodAsCodeStringOn:  theStream indent: anIS]. 

		self allowedRootTypesPersistenceAsCodeStringOn: theStream indent: anIS.
		self allowedSubTypesPersistenceAsCodeStringOn: theStream indent: anIS.

		theStream
	     	nextPutAll: anIS;  nextPutAll: '('; nextPutAll: (self pcForV: self  class partsPersistenceSymbol); cr
	].

	self parts do:  [:aPart |
		aPart isGroup 
			ifTrue: [ 
				aPart inner: true persistenceAsCodeStringChunks: theCollection on: theStream 
					filter: theFilter indent: anIS , self indentStringForPersistenceAsCode doSubChunks: theDoSubChunks
			]
			ifFalse: [ 
				aPart persistenceAsCodeStringOn: theStream indent: anIS , self indentStringForPersistenceAsCode
			]
	].
	
	theStream isNil ifFalse: [  theStream  nextPutAll: anIS; nextPutAll: ' )' ; cr; cr]!

mustStoreWithFilter: theFilter

	^theFilter = true or: [ 
			(theFilter = #dirty and: [ self isDirty]) or: [ 
				(self preferredInstallerClass existsGroupMethod: self storeMethodSelector inClass: self storeClassName) not or: [ 
					theFilter = #source and: [ 
						(self preferredInstallerClass  sameSource: self persistenceAsCodeString
							groupMethod: self storeMethodSelector asSymbol inClass: self storeClassName asSymbol) not
					]
				]
			]
		]!

persistenceAsCodeString

	| aCollection |

	aCollection := self persistenceAsCodeStringChunksFilter: true doSubChunks: false.
	(aCollection isNil or: [ aCollection isEmpty]) ifTrue: [ ^nil].

	^aCollection first at: 2!

persistenceAsCodeStringChunks

	^self persistenceAsCodeStringChunksFilter: nil doSubChunks: true!

persistenceAsCodeStringChunksFilter: theFilter doSubChunks: theDoSubChunks

	| aStream aCollection aChunk aFilter anIS aSep aRes aMustStore |

	aFilter := theFilter = true ifTrue: [ true] ifFalse: [ theFilter = #dirty ifTrue: [ #dirty] ifFalse: [ theFilter = #source ifTrue: [ #source] ifFalse: [ nil]]].

	aCollection := OrderedCollection new: 32.
	aStream := nil.


	anIS := self indentStringForPersistenceAsCode.
	aSep := self separatorForPersistenceAsCode.
	
	aMustStore := self mustStoreWithFilter: theFilter.

	aMustStore ifTrue: [ 
		aStream := WriteStream on: (String new: (self partsSize + 1) * 256).
		aChunk := Array with: self with: aStream.
		aCollection add: aChunk.

		aStream nextPutAll: anIS; nextPutAll: '#( ';  nextPutAll: (self pcForV: self kind);  
			nextPutAll: aSep; nextPutAll: self name printString; cr.
	].

	self localValuesPersistenceAsCodeStringChunks: aCollection on: aStream filter: aFilter indent: anIS doSubChunks: theDoSubChunks.

	aMustStore ifTrue: [ 
		aStream nextPutAll: anIS; nextPutAll: ' )' ; cr; cr.
	].

	aRes := aCollection collect: [:aCh | Array with: aCh first with: aCh last contents].
	^aRes!

rebindReferencedAllowedRootTypesValuesFromSolver: theSolver
	
	| someTypes someTypesRefTmpValues |

	(allowedRootTypesRefsTmpValues isNil or: [ allowedRootTypesRefsTmpValues size < 2]) ifTrue: [ ^self].

	someTypes := OrderedCollection new: allowedRootTypesRefsTmpValues size - 1.

	someTypesRefTmpValues := OrderedCollection new: allowedRootTypesRefsTmpValues size - 1.
	someTypesRefTmpValues addAll: (allowedRootTypesRefsTmpValues copyFrom: 2 to: allowedRootTypesRefsTmpValues size).

	someTypesRefTmpValues copy do: [:aTypeRefValues |  | aType |
		(aTypeRefValues isNil not and: [ aTypeRefValues isEmpty not]) ifTrue:  [
			aType := CODEElement resolveReferencedTypeFromPersistenceAsCode: aTypeRefValues solver: theSolver.
			aType isNil ifFalse: [ 
				someTypes add: aType.
				someTypesRefTmpValues remove: aTypeRefValues
			]
		]
	].

	someTypes isEmpty ifFalse: [ 
		someTypes do: [:aType | self allowedRootTypesAdd: aType]
	].
	someTypesRefTmpValues isEmpty 
		ifTrue: [ allowedRootTypesRefsTmpValues := nil]
		ifFalse: [ allowedRootTypesRefsTmpValues := someTypesRefTmpValues].!

rebindReferencedAllowedSubTypesValuesFromSolver: theSolver
	
	| someTypes someTypesRefTmpValues |

	(allowedSubTypesRefsTmpValues isNil or: [ allowedSubTypesRefsTmpValues size < 2]) ifTrue: [ ^self].

	someTypes := OrderedCollection new: allowedSubTypesRefsTmpValues size - 1.

	someTypesRefTmpValues := OrderedCollection new: allowedSubTypesRefsTmpValues size - 1.
	someTypesRefTmpValues addAll: (allowedSubTypesRefsTmpValues copyFrom: 2 to: allowedSubTypesRefsTmpValues size).

	someTypesRefTmpValues copy do: [:aTypeRefValues |  | aType |
		(aTypeRefValues isNil not and: [ aTypeRefValues isEmpty not]) ifTrue:  [
			aType := CODEElement resolveReferencedTypeFromPersistenceAsCode: aTypeRefValues solver: theSolver.
			aType isNil ifFalse: [ 
				someTypes add: aType.
				someTypesRefTmpValues remove: aTypeRefValues
			]
		]
	].

	someTypes isEmpty ifFalse: [ 
		someTypes do: [:aType | self allowedSubTypesAdd: aType]
	].
	someTypesRefTmpValues isEmpty 
		ifTrue: [ allowedSubTypesRefsTmpValues := nil]
		ifFalse: [ allowedSubTypesRefsTmpValues := someTypesRefTmpValues].!

rebindToModel
	
	self bindToReferencedModel.

	model isNil ifFalse: [ self rebindToModelFromSolver: model]!

rebindToModelFromSolver: theSolver

	self rebindReferencedAllowedRootTypesValuesFromSolver: theSolver.
	self rebindReferencedAllowedSubTypesValuesFromSolver: theSolver.

	self parts do: [:aPart | aPart rebindToModelFromSolver: theSolver].!

registerNewLayoutRectangle: theLayoutRectangle
	theLayoutRectangle isNil ifTrue: [ ^self].

	layoutRectangles := layoutRectangles , (Array with: theLayoutRectangle)!

unbindFromModel

	self initModelRefTmpValuesFromModel.
	self initAllowedRootTypesRefsTmpValuesFromModel.
	self initAllowedSubTypesRefsTmpValuesFromModel.

	self partsPrivate do: [:aPart | aPart unbindFromModel].

	self forzeModel: nil.
	self forzeAllowedRootTypes: nil.
	self forzeAllowedSubTypes: nil.

	self partsPrivate do: [:aPart | aPart forgetMetaInfo].! !

!CMDefinedWindow publicMethodsFor: 'semantic checking'!

checkAllowedRootTypesAdd: aValue
	"Generated by ISF/AD. Do not modify"
	^true!

checkAllowedRootTypesRemove: aValue
	"Generated by ISF/AD. Do not modify"
	^true!

checkAllowedSubTypesAdd: aValue
	"Generated by ISF/AD. Do not modify"
	^true!

checkAllowedSubTypesRemove: aValue
	"Generated by ISF/AD. Do not modify"
	^true! !

!CMDefinedWindow publicMethodsFor: 'semantic links'!

allowedRootTypesLinkCreate
	"Generated by ISF/AD. Do not modify"
	| anObject |

	anObject := self allowedRootTypesCreate.
	(RTProfiledStringRequestor new requestNameFor: anObject)
		ifTrue: [^self allowedRootTypesAdd: anObject]!

allowedRootTypesLinkSelect
	"Generated by ISF/AD. Do not modify"
	| anObject |

	anObject := RTObjectServer new
		selectIn: self allowedRootTypesCandidates
		initially: nil
		withLabel: 'Select...'
		ifUnchanged: [^self].
	self allowedRootTypesAdd: anObject.!

allowedRootTypesLinkSelectOrCreate
	"Generated by ISF/AD. Do not modify"
	| anObject |

	anObject := RTObjectServer new
		chooseOrNewIn: self allowedRootTypesCandidates
		initially: nil
		label: 'Select Or Create'
		class: self allowedRootTypesCreate class
		ifUnchanged: [^self].
	self allowedRootTypesAdd: anObject!

allowedRootTypesScope
	"Generated by ISF/AD. Do not modify"
	^nil!

allowedSubTypesCandidates
	"Generated by ISF/AD. Do not modify"
	^self allowedSubTypes asArray!

allowedSubTypesCreate
	"Generated by ISF/AD. Do not modify"
	^CODEParameter new!

allowedSubTypesLinkCreate
	"Generated by ISF/AD. Do not modify"
	| anObject |

	anObject := self allowedSubTypesCreate.
	(RTProfiledStringRequestor new requestNameFor: anObject)
		ifTrue: [^self allowedSubTypesAdd: anObject]!

allowedSubTypesLinkSelect
	"Generated by ISF/AD. Do not modify"
	| anObject |

	anObject := RTObjectServer new
		selectIn: self allowedSubTypesCandidates
		initially: nil
		withLabel: 'Select...'
		ifUnchanged: [^self].
	self allowedSubTypesAdd: anObject.!

allowedSubTypesLinkSelectOrCreate
	"Generated by ISF/AD. Do not modify"
	| anObject |

	anObject := RTObjectServer new
		chooseOrNewIn: self allowedSubTypesCandidates
		initially: nil
		label: 'Select Or Create'
		class: self allowedSubTypesCreate class
		ifUnchanged: [^self].
	self allowedSubTypesAdd: anObject!

allowedSubTypesScope
	"Generated by ISF/AD. Do not modify"
	^nil! !

!CMDefinedWindow publicMethodsFor: 'semantic links-custom'!

allowedRootTypesCandidates
	| someTypes |
	self model isNil ifTrue: [ ^nil].
	someTypes := self model allTypes.
	self allowedRootTypesPrivate do: [:aType |
		someTypes remove: aType
	].
	^someTypes!

allowedRootTypesCreate

	| someTypes aSelectedType |

	someTypes := self allowedRootTypesCandidates.
	(someTypes isNil or: [ someTypes isEmpty]) ifTrue: [ 
		Dialog warn: 'No Types in Model candidate as ValueType of Attribute\' withCRs, self name.
		^nil
	].

	someTypes := someTypes asSortedCollection: [:a :b | a nlsName < b nlsName].

	aSelectedType := Dialog 
		choose: ('	Please, select a Type as ValueType of Attribute	\			', self name) withCRs
		fromList: (someTypes collect: [:aType | 
			Object errorSignal
				handle: [:anException | anException returnWith: aType name]
				do: [ aType nlsName, ' 	(', aType name, ')']]) 
		values: someTypes 
		lines: (((someTypes size + 1) max: 5) min: 18)
		cancel: [nil]
		initialSelection: nil.
	aSelectedType isNil ifTrue: [ ^nil].

	self allowedRootTypesAdd: aSelectedType.

	^aSelectedType!

modelCandidates

	^CODEModel allModelsInSystem!

modelLinkSelect
	| someModels aSelectedModel |

	someModels := self modelCandidates.
	(someModels isNil or: [ someModels isEmpty]) ifTrue: [ 
		Dialog warn: 'No Models in System candidate as ViewModel of Map\' withCRs, self name.
		^nil
	].

	someModels := someModels asSortedCollection: [:a :b | a name < b name].

	aSelectedModel := Dialog 
		choose: ('Please, select a Model as Model of DefinedWindow\', self name) withCRs
		fromList: (someModels collect: [:aModel | 
			Object errorSignal
				handle: [:anException | anException returnWith: aModel name]
				do: [ aModel displayName ]]) 
		values: someModels 
		lines: (((someModels size + 1) max: 5) min: 18)
		cancel: [nil].
	aSelectedModel isNil ifTrue: [ ^nil].

	self unbindFromModel.
	self model: aSelectedModel.
	self rebindToModel.
	self initModelRefTmpValuesFromModel.

	^aSelectedModel!

xallowedRootTypesCreate

	| someTypes aSelectedType |

	someTypes := self allowedRootTypesCandidates.
	(someTypes isNil or: [ someTypes isEmpty]) ifTrue: [ 
		Dialog warn: 'No Types in Model candidate as ValueType of Attribute\' withCRs, self name.
		^nil
	].

	someTypes := someTypes asSortedCollection: [:a :b | a nlsName < b nlsName].

	aSelectedType := Dialog 
		choose: ('	Please, select a Type as ValueType of Attribute	\			', self name) withCRs
		fromList: (someTypes collect: [:aType | 
			Object errorSignal
				handle: [:anException | anException returnWith: aType name]
				do: [ aType nlsName, ' 	(', aType name, ')']]) 
		values: someTypes 
		lines: (((someTypes size + 1) max: 5) min: 18)
		cancel: [nil]
		initialSelection: nil.
	aSelectedType isNil ifTrue: [ ^nil].

	self allowedRootTypesAdd: aSelectedType.

	^aSelectedType! !

!CMDefinedWindow publicMethodsFor: 'tests'!

isWindow
	^true!

reportSubPartsHaveSameLayoutGroups

	| aLayoutGroup  |
	aLayoutGroup := nil.
	self parts do: [:aPart | | aRes |
		aLayoutGroup isNil 
			ifTrue: [ aLayoutGroup := aPart layoutGroup ]
			ifFalse: [   
				aLayoutGroup = aPart layoutGroup ifFalse: [ ^Array with: self with: aPart].
				aPart isGroup ifTrue: [ 
					aRes := aPart testIfAllSubPartsHaveSameLayoutGroups.
					aRes isNil ifFalse: [ ^aRes]]]].
	^nil!

testIfAllSubPartsHaveSameLayoutGroups

	| aLayoutGroup  |
	aLayoutGroup := nil.
	self parts do: [:aPart | | |
		aLayoutGroup isNil 
			ifTrue: [ aLayoutGroup := aPart layoutGroup ]
			ifFalse: [ aLayoutGroup = aPart layoutGroup ifFalse: [ 
Transcript show: 'SubParts NOT SAME LayoutGroup '; show: self name; show: ' window'; cr.
^false].
		aPart isGroup ifTrue: [ aPart  testIfAllSubPartsHaveSameLayoutGroups ifFalse: [ ^false]]]].
	^true! !

CMDefinedPart initializeAfterLoad!
CMDefinedPartWithLayoutRectangle initializeAfterLoad!
CMDefinedPartAnonymous initializeAfterLoad!
CMDefinedPartCode initializeAfterLoad!
CMDefinedPartLongField initializeAfterLoad!
CMDefinedPartColumn initializeAfterLoad!
CMDefinedPartField initializeAfterLoad!
CMDefinedPartGroup initializeAfterLoad!
CMDefinedPartImage initializeAfterLoad!
CMDefinedPartList initializeAfterLoad!
CMDefinedPartMultiList initializeAfterLoad!
CMDefinedPartSep initializeAfterLoad!
CMDefinedPartSlider initializeAfterLoad!
CMDefinedPartXYSlider initializeAfterLoad!
CMDefinedPartTranscript initializeAfterLoad!
CMDefinedPartWithNLS initializeAfterLoad!
CMDefinedPartButton initializeAfterLoad!
CMDefinedPartSwitch initializeAfterLoad!
CMDefinedPartTitle initializeAfterLoad!
CMDefinedWindow initializeAfterLoad!
CMDefinedPartsInstaller initializeAfterLoad!
CMDefinedPanelAppModel initializeAfterLoad!
CODE_META_DefinedUI initializeAfterLoad!

CODE_META_DefinedUI loaded!
