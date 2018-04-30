'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:54 pm'!



Application create: #CODE_META_APP with: 
    (#( CODEgenUI CODE_META)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

CODE_META_APP becomeDefault!

Object subclass: #CMAPDefinitionsHolderFactory
	instanceVariableNames: 'manager '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

Object subclass: #CMAPManager
	instanceVariableNames: 'defaultPath pilot userConfigurationsCollection userConfiguration applicationConfiguration definitionsHolderFactory editorsOpener logged userName lastLogoutTime project '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

Object subclass: #CMAPProject
	instanceVariableNames: 'name folder domainCMGO applicationConfiguration '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

Object subclass: #CMAPService
	instanceVariableNames: 'applicationSession project '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

Object subclass: #CMAPSession
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CMAPSession subclass: #CMAPApplicationSession
	instanceVariableNames: 'userSession applicationConfiguration service ui '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CMAPSession subclass: #CMAPLoginSession
	instanceVariableNames: 'userConfigurationsCollection userSessions '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CMAPSession subclass: #CMAPUserSession
	instanceVariableNames: 'loginSession userConfiguration applicationConfigurationsCollection applicationSessions '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CMInfoPersistencyHolder subclass: #CMAPInfoHolder
	instanceVariableNames: 'applicationConfiguration '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CMMetaInfoPersistencyHolder subclass: #CMAPMetaInfoHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CMDefinitionsHolder subclass: #CMAPDefinitionsHolder
	instanceVariableNames: 'applicationConfigurationHolder '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CODEMModelEditorsOpener subclass: #CMAPEditorsOpener
	instanceVariableNames: 'manager '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CODEMModelMETAConfiguration subclass: #CMAPMETAConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

METAVirtualConfigurationSpecialization subclass: #CMAPApplicationConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

METAVirtualConfigurationSpecialization subclass: #CMAPDeveloperConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

METAVirtualConfigurationSpecialization subclass: #CMAPLoginConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

METAVirtualConfigurationSpecialization subclass: #CMAPUserConfiguration
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

METAConfigurationsCollection subclass: #CMAPApplicationConfigurationsCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

METAConfigurationsCollection subclass: #CMAPLoginConfigurationsCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

METAConfigurationsCollection subclass: #CMAPUserConfigurationsCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CODEMModelConfigurationsCollection subclass: #CMAPConfigurationsCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

ApplicationModel subclass: #CMAPLauncherPanel
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

ApplicationModel subclass: #CMAPPilotMessagesPanel
	instanceVariableNames: 'messageView '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

ApplicationModel subclass: #CMAPPilotWindowsPanel
	instanceVariableNames: 'pilot windowsList inTransition '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CODEMModelPathFinderGenericBrowser subclass: #CMAPPathFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CODEMModelConfigurationsBrowser subclass: #CMAPConfigurationsBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

CODEMModelGenericBrowser subclass: #CMAPApplicationBrowser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

RTApplicationModel subclass: #CMAPPilot
	classInstanceVariableNames: 'multiButtonsBar justReturnedFromSnapshot '
	instanceVariableNames: 'messageView defaultPath panelsModel panelsTabsModel alreadyUpdatedAfterOpening applicationConfiguration manager '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

Application subclass: #CODE_META_APP
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

TranslationsPersistencyHolder subclass: #CMAPTranslationHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_APP becomeDefault!

!CMAPApplicationBrowser class publicMethodsFor: 'class initialization'!

initialize
	"CMAPApplicationBrowser initialize.
	CMAPApplicationBrowser allSubclasses do: [:aClass | aClass initialize]"

	super initialize! !

!CMAPApplicationBrowser class publicMethodsFor: 'instance creation'!

browserKind
	^#CMAPApplicationBrowser!

checkedBrowserParameters:	theBrowserParameters
	| aDict |

	aDict := theBrowserParameters isNil ifTrue: [ Dictionary new] ifFalse: [ theBrowserParameters].

	self in: aDict at: METABrowser showCanvasLabelParameterSymbol 			ifAbsentPut: true.

	aDict at: METABrowser editorsOpenerParameterSymbol put: CMAPEditorsOpener editorsOpener.
	
	^super checkedBrowserParameters:	theBrowserParameters.! !

!CMAPApplicationBrowser class publicMethodsFor: 'interface specs'!

aboutSpec
	^CMAPLauncherPanel autorCanvasSpec!

technicalSupportSpec

	^CMAPLauncherPanel soporteCanvasSpec!

windowSpec

	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min:#(#Point 400 300 ) #bounds: #(#Rectangle 32 32 932 732 ) #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!CMAPApplicationBrowser class publicMethodsFor: 'menu'!

assistantDebugMenu
	"self assistantDebugMenu startUp"

	^super assistantDebugMenu!

assistantRuntimeMenu
	"self assistantRuntimeMenu startUp"

	^super assistantDebugMenu!

helpMenu
	"self helpMenu startUp"

	^#(#HWHelpPopUpMenu #('A_yuda' '_Autor' '_Soporte') #(1 2 ) #( #help #about #techSupport) #('Obtener Ayuda acerca de OMGCMAPware' 'Informacion acerca del Autor, Derechos y Limitaciones de Uso de OMGCMAPware' 'Obtener soporte acerda de OMGCMAPware') ) decodeAsLiteralArray!

metaApplicationBrowserDebugMenu
	"self metaApplicationBrowserDebugMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador'  'Formato' '_Grabadora' '_Ventanas' '_Ayuda' 'Acceso Desarrollador') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Formato' '_Grabadora' '_Ventanas' 'Herramientas' '_Ayuda' 'Acceso Desarrollador') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' ''  '')].


	^aMenu!

metaApplicationBrowserMenu
	^DEBUGDvpt
		ifTrue: [ self metaApplicationBrowserDebugMenu]
		ifFalse: [ self metaApplicationBrowserRuntimeMenu]!

metaApplicationBrowserRuntimeMenu
	"self metaApplicationBrowserRuntimeMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Formato' '_Grabadora' '_Ventanas' '_Ayuda') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					yourself)
				helps: #(''  ''  '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Navegador' 'Formato' '_Grabadora' '_Ventanas' 'Herramientas' '_Ayuda') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					yourself)
				helps: #(''  '' '' '' '' ''  )].

	^aMenu!

pathMenu
	"self pathMenu startUp"

	^#(#HWHelpPopUpMenu #('_Desde el principio' '_Volver Atras' '_Abrir ventana de camino' '_Cerrar  ventana de camino' ) #(2 ) #(#goToFirstInHistory #backOneInHistory #showPathWindow #hidePathWindow ) #('Mostrar el primer objeto en el camino' 'Mostrar objeto anterior en el camino' 'Abrir ventana de camino' 'Cerrar ventana de camino' ))  decodeAsLiteralArray!

viewsMenu
	"self viewsMenu startUp"

	^#(#HWHelpPopUpMenu #('_Solo Navegador' 'Solo Editor' '_Navegador y Editor' '-- no usada --'  '_Formato Automatico' ) #(3 ) #(#onlyDisplayLists #onlyDisplayEditor #displayListAndEditor #showInheritedMode #autoLayoutMode) #( 'Mostrar solamente el panel de navegacion de caminos' 'Mostrar solamente el panel de edicion de datos' 'Mostrar los dos paneles : navegacio y editor' '-- no usado --' 'Modo Formato Automatico' ) )  decodeAsLiteralArray!

windowsMenu
	"self windowsMenu startUp"

	^#(#HWHelpPopUpMenu #('_Refrescar' '_Mover' '_Dimensiones' '_Delante' '_Detras' '_Colapsar' 'Lista de Ventanas') #(6) #( #refreshWindow #moveWindow #resizeWindow #frontWindow #backWindow #collapseWindow  #windowList ) #('Refrescar la visualizacion completa de la pantalla (no vuelve a acceder a los datos)' 'Mover la Pantalla' 'Cambiar las dimensiones de la pantalla' 'Traer la ventana delante de las demas' 'Enviar la ventana detras de todas las demas' 'Colapsar la ventana. Puede recuperarla desde su barra de herramientas windows'  'Mostrar una lista de todas las ventanas de OMGCMAPware') )  decodeAsLiteralArray!

windowsRaiseHelpString
	^'Mostrar ventana ' copy! !

!CMAPApplicationBrowser class publicMethodsFor: 'menu indexed accessing'!

autoLayoutModeMenuItemHelp
	^'Cambia el modo de formato a automatico : el navegador ocupara toda la pantalla en ausencia de seleccion' copy!

autoLayoutModeMenuItemLabel
	^'Formato Automatico' copy!

fixedLayoutModeMenuItemHelp
	^'Cambia el modo de formato a fijo : el navegador mantiene sus dimensiones haya o no seleccion' copy!

fixedLayoutModeMenuItemLabel
	^'Formato Fijo' copy!

hideInheritedMenuItemHelp
	^'-- no usada --' copy!

hideInheritedMenuItemLabel
	^'-- no usada --' copy!

showInheritedMenuItemHelp
	^'-- no usada --' copy!

showInheritedMenuItemLabel
	^'-- no usada --' copy! !

!CMAPApplicationBrowser class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsBrowserClass
	^CMAPConfigurationsBrowser!

preferredEditorsOpenerClass
	^CMAPEditorsOpener!

preferredMETAConfigurationClass

	^CMAPMETAConfiguration! !

!CMAPApplicationBrowser publicMethodsFor: 'label'!

browserLabelTitlePrefix
	^'OMGCMAPware: ' copy! !

!CMAPApplicationBrowser publicMethodsFor: 'menu'!

about
	OMGCMAPLauncherPanel openWithSpec: #autorWindowSpec!

help

	Dialog warn: 
		'Por favor, contacte Soporte tecnico\para realizar cualquier consulta\y recibir un manual de instrucciones'
		withCRs.
	self techSupport!

techSupport
	OMGCMAPLauncherPanel openWithSpec: #soporteWindowSpec! !

!CMAPApplicationBrowser publicMethodsFor: 'updating'!

updateWindowLabel
	| aLabel aBrowserParameters aPrefix aPrefixString |
	aLabel :=  (self readOnly 
		ifTrue: ['Read Only ', self browserLabelTitlePrefix] 
		ifFalse: [self browserLabelTitlePrefix]),
		objectHolder classAndName.


	aBrowserParameters := objectHolder browserParameters.

	aPrefix := aBrowserParameters isNil
		ifTrue: ['']
		ifFalse: [ 
			aPrefixString := aBrowserParameters at: METABrowser windowLabelPrefixSymbol ifAbsent: [nil].
			aPrefixString isNil
				ifTrue: ['']
				ifFalse: [aPrefixString, ' ']].


	self builder window label: aPrefix ,  aLabel.
	self isDialog ifTrue: [ self dialogLabel value: 'Selection ',aPrefix , aLabel].!

updateWindowsList

	self updateWindowsListMenu!

updateWindowsListMenu

	| aMenu aWindowsListMenu |

	aMenu := self metaApplicationBrowserMenu.
	aMenu isNil ifTrue: [ ^self].

	aWindowsListMenu := aMenu valueAt: self class windowsSubMenuIndexInMenu.
	aWindowsListMenu buildMenu.! !

!CMAPApplicationConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"CMAPApplicationConfiguration initialize"! !

!CMAPApplicationConfiguration class publicMethodsFor: 'current'!

copyCurrent
		self shouldNotImplement!

current
	self shouldNotImplement!

current: theConfiguration
	self shouldNotImplement! !

!CMAPApplicationConfiguration class publicMethodsFor: 'examples'!

exampleApplicationConfiguration01

	| aConfiguration someParameters aCopy |

	aConfiguration := self new.

	aConfiguration name: self configurationName.
	aConfiguration description: self configurationDescription.

	someParameters := self configurationParameters.
	someParameters do: [:aParameter | aConfiguration parametersAdd: aParameter].

	aConfiguration recalculateParameters.

	aCopy := aConfiguration copyConfiguration.

	aCopy set: self applicationNameParameterName value: 'SampleApp01' copy.
	aCopy set: self modelKindLabelParameterName value: 'SampleBusinesModel01' copy.
	aCopy set: self projectClassNameParameterName value: CMAPProject name.
	aCopy set: self infoHolderClassNameParameterName value: CMAPInfoHolder name.
	aCopy set: self metaInfoHolderClassNameParameterName value: CMAPMetaInfoHolder name.
	aCopy set: self metaInfoStoreMethodSelectorParameterName value: #sampleBusinessMetamodel01.
	aCopy set: self translationHolderClassNameParameterName value: CMAPTranslationHolder name.
	aCopy set: self translationStoreMethodSelectorParameterName value: #sampleBusinessTranslation01.


	aCopy set: self definitionsHolderFactoryClassNameParameterName value: CMAPDefinitionsHolderFactory name.
	aCopy set: self editorsOpenerClassNameParameterName value: CMAPEditorsOpener name.
	aCopy set: self browserClassNameParameterName value: CMAPPathFinder name.

	aCopy set: self savePostfixParameterName value: '.sample01'.

	aCopy set: self developerConfigurationParameterName value: CMAPDeveloperConfiguration exampleDeveloperConfiguration01.

	aCopy recalculateParameters.

	^aCopy!

exampleGenericApplicationConfiguration01

	| aConfiguration someParameters aCopy |

	aConfiguration := self new.

	aConfiguration name: self configurationName.
	aConfiguration description: self configurationDescription.

	someParameters := self configurationParameters.
	someParameters do: [:aParameter | aConfiguration parametersAdd: aParameter].

	aConfiguration recalculateParameters.

	aCopy := aConfiguration copyConfiguration.

	aCopy set: self applicationNameParameterName value: 'SampleApp01' copy.
	aCopy set: self modelKindLabelParameterName value: 'SampleBusinesModel01' copy.
	aCopy set: self projectClassNameParameterName value: CMAPProject name.
	aCopy set: self infoHolderClassNameParameterName value: CMAPInfoHolder name.
	aCopy set: self metaInfoHolderClassNameParameterName value: CMAPMetaInfoHolder name.
	aCopy set: self metaInfoStoreMethodSelectorParameterName value: #sampleBusinessMetamodel01.
	aCopy set: self translationHolderClassNameParameterName value: CMAPTranslationHolder name.
	aCopy set: self translationStoreMethodSelectorParameterName value: #sampleBusinessTranslation01.


	aCopy set: self definitionsHolderFactoryClassNameParameterName value: CMAPDefinitionsHolderFactory name.
	aCopy set: self editorsOpenerClassNameParameterName value: CMAPEditorsOpener name.
	aCopy set: self browserClassNameParameterName value: CMAPPathFinder name.

	aCopy set: self savePostfixParameterName value: '.sample01'.

	aCopy set: self developerConfigurationParameterName value: CMAPDeveloperConfiguration exampleDeveloperConfiguration01.

	aCopy recalculateParameters.

	^aCopy! !

!CMAPApplicationConfiguration class publicMethodsFor: 'instance creation'!

initialConfiguration
	self shouldNotImplement!

installCurrent
	self shouldNotImplement! !

!CMAPApplicationConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^('Configuracion raiz de una Aplicacion\', 
		'El Desarrollador utiliza parametros en esta configuracion para especificar los componentes y constantes que ensamblan una Aplicacion') copy withCRs!

configurationName
	^'Parametros de Configuracion de Aplicacion' copy!

configurationParameters
 
	^(OrderedCollection new: 32)
		add: self applicationNameParameter;
		add: self browserClassNameParameter;
		add: self defaultProjectNameParameter;
		add: self definitionsHolderFactoryClassNameParameter;
		add: self domainFactoryMethodSelectorParameter;
		add: self domainNameParameter;
		add: self editorsOpenerClassNameParameter;
		add: self homeNameParameter;
		add: self infoHolderClassNameParameter;
		add: self infoStoreMethodSelectorParameter;
		add: self metaInfoHolderClassNameParameter;
		add: self metaInfoStoreMethodSelectorParameter;
		add: self modelKindLabelParameter;
		add: self projectClassNameParameter;
		add: self rootNameAttributeNameParameter;
		add: self rootNameParameter;
		add: self savePostfixParameter;
		add: self translationHolderClassNameParameter;
		add: self translationStoreMethodSelectorParameter;
		add: self developerConfigurationParameter;
		yourself! !

!CMAPApplicationConfiguration class publicMethodsFor: 'ref:parameters'!

applicationNameParameter
	^self preferredParameterStringClass
			name: self applicationNameParameterName
			label: 'Application name'
			value: self applicationNameParameterValue
			isEditable: true
			description: ('Application name') withCRs
			defaultValue: self applicationNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browserClassNameParameter
	^self preferredParameterSymbolClass
			name: self browserClassNameParameterName
			label: 'Browser class name'
			value: self browserClassNameParameterValue
			isEditable: true
			description: ('Class to create browsers') withCRs
			defaultValue: self browserClassNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

defaultProjectNameParameter
	^self preferredParameterStringClass
			name: self defaultProjectNameParameterName
			label: 'Project default name'
			value: self defaultProjectNameParameterValue
			isEditable: true
			description: ('Nombre por defecto de Proyecto') withCRs
			defaultValue: self defaultProjectNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

definitionsHolderFactoryClassNameParameter
	^self preferredParameterSymbolClass
			name: self definitionsHolderFactoryClassNameParameterName
			label: 'DefinitionsHolderFactoryClass class name'
			value: self definitionsHolderFactoryClassNameParameterValue
			isEditable: true
			description: ('Class to create a factory for definitions holders') withCRs
			defaultValue: self definitionsHolderFactoryClassNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

developerConfigurationParameter
	^self preferredParameterSubConfigurationClass
			name: self developerConfigurationParameterName
			label: 'Developer Configuration'
			value: self developerConfigurationParameterValue
			isEditable: true
			description: ('Developer Configuration') withCRs
			defaultValue: self developerConfigurationParameterValue
			verificationBlock: nil
			derivationBlock: nil!

domainFactoryMethodSelectorParameter
	^self preferredParameterSymbolClass
			name: self domainFactoryMethodSelectorParameterName
			label: 'Domain Factory Method Selector'
			value: self domainFactoryMethodSelectorParameterValue
			isEditable: true
			description: ('Method to create, save and retrieve information Domains') withCRs
			defaultValue: self domainFactoryMethodSelectorParameterValue
			verificationBlock: nil
			derivationBlock: nil!

domainNameParameter
	^self preferredParameterStringClass
			name: self domainNameParameterName
			label: 'Domain name'
			value: self domainNameParameterValue
			isEditable: true
			description: ('Domain name') withCRs
			defaultValue: self domainNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

editorsOpenerClassNameParameter
	^self preferredParameterSymbolClass
			name: self editorsOpenerClassNameParameterName
			label: 'EditorsOpener class name'
			value: self editorsOpenerClassNameParameterValue
			isEditable: true
			description: ('Class to create a factory for definitions holders') withCRs
			defaultValue: self editorsOpenerClassNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

homeNameParameter
	^self preferredParameterStringClass
			name: self homeNameParameterName
			label: 'Home name'
			value: self homeNameParameterValue
			isEditable: true
			description: ('Home name') withCRs
			defaultValue: self homeNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

infoHolderClassNameParameter
	^self preferredParameterSymbolClass
			name: self infoHolderClassNameParameterName
			label: 'InfoHolder class name'
			value: self infoHolderClassNameParameterValue
			isEditable: true
			description: ('Class to create, save and retrieve information Domains') withCRs
			defaultValue: self infoHolderClassNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

infoStoreMethodSelectorParameter
	^self preferredParameterSymbolClass
			name: self infoStoreMethodSelectorParameterName
			label: 'InfoStoreMethodSelector '
			value: self infoStoreMethodSelectorParameterValue
			isEditable: true
			description: ('Selector to create, save and retrieve information') withCRs
			defaultValue: self infoStoreMethodSelectorParameterValue
			verificationBlock: nil
			derivationBlock: nil!

metaInfoHolderClassNameParameter
	^self preferredParameterSymbolClass
			name: self metaInfoHolderClassNameParameterName
			label: 'MetaInfoHolder class name'
			value: self metaInfoHolderClassNameParameterValue
			isEditable: true
			description: ('Class to create, save and retrieve metaInformation') withCRs
			defaultValue: self metaInfoHolderClassNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

metaInfoStoreMethodSelectorParameter
	^self preferredParameterSymbolClass
			name: self metaInfoStoreMethodSelectorParameterName
			label: 'MetaInfoStoreMethodSelector '
			value: self metaInfoStoreMethodSelectorParameterValue
			isEditable: true
			description: ('Selector to create, save and retrieve metaInformation') withCRs
			defaultValue: self metaInfoStoreMethodSelectorParameterValue
			verificationBlock: nil
			derivationBlock: nil!

modelKindLabelParameter
	^self preferredParameterStringClass
			name: self modelKindLabelParameterName
			label: 'Project class name'
			value: self modelKindLabelParameterValue
			isEditable: true
			description: ('Etiqueta de tipo de modelo') withCRs
			defaultValue: self modelKindLabelParameterValue
			verificationBlock: nil
			derivationBlock: nil!

projectClassNameParameter
	^self preferredParameterSymbolClass
			name: self projectClassNameParameterName
			label: 'Project class name'
			value: self projectClassNameParameterValue
			isEditable: true
			description: ('Class to instantiate Projects') withCRs
			defaultValue: self projectClassNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

rootNameAttributeNameParameter
	^self preferredParameterStringClass
			name: self rootNameAttributeNameParameterName
			label: 'Root name Attribute Name'
			value: self rootNameAttributeNameParameterValue
			isEditable: true
			description: ('Root name Attribute Name') withCRs
			defaultValue: self rootNameAttributeNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

rootNameParameter
	^self preferredParameterStringClass
			name: self rootNameParameterName
			label: 'Root name'
			value: self rootNameParameterValue
			isEditable: true
			description: ('Root name') withCRs
			defaultValue: self rootNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

savePostfixParameter
	^self preferredParameterStringClass
			name: self savePostfixParameterName
			label: 'Project class name'
			value: self savePostfixParameterValue
			isEditable: true
			description: ('Etiqueta de tipo de modelo') withCRs
			defaultValue: self savePostfixParameterValue
			verificationBlock: nil
			derivationBlock: nil!

translationHolderClassNameParameter
	^self preferredParameterSymbolClass
			name: self translationHolderClassNameParameterName
			label: 'MetaInfoHolder class name'
			value: self translationHolderClassNameParameterValue
			isEditable: true
			description: ('Class to create, save and retrieve metainfo translations') withCRs
			defaultValue: self translationHolderClassNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

translationStoreMethodSelectorParameter
	^self preferredParameterSymbolClass
			name: self translationStoreMethodSelectorParameterName
			label: 'TranslationStoreMethodSelector '
			value: self translationStoreMethodSelectorParameterValue
			isEditable: true
			description: ('Selector to create, save and retrieve translation') withCRs
			defaultValue: self translationStoreMethodSelectorParameterValue
			verificationBlock: nil
			derivationBlock: nil! !

!CMAPApplicationConfiguration class publicMethodsFor: 'ref:parametersvalues'!

applicationNameParameterName
	^#applicationName!

applicationNameParameterValue
	^'CMAPware' copy!

browserClassNameParameterName
	^#browserClassName!

browserClassNameParameterValue
	^#CMAPPathName!

defaultProjectNameParameterName
	^#defaultProjectName!

defaultProjectNameParameterValue
	^'Project' copy!

definitionsHolderFactoryClassNameParameterName
	^#definitionsHolderFactoryClassName!

definitionsHolderFactoryClassNameParameterValue
	^#CMAPDefinitionsHolderFactory!

developerConfigurationParameterName
	^#developerConfiguration!

developerConfigurationParameterValue
	^CMAPDeveloperConfiguration current!

domainFactoryMethodSelectorParameterName
	^#domainFactoryMethodSelector!

domainFactoryMethodSelectorParameterValue
	^#newDomainWithApplicationConfiguration:!

domainNameParameterName
	^#domainName!

domainNameParameterValue
	^'CMAPware Domain' copy!

editorsOpenerClassNameParameterName
	^#editorsOpenerClassName!

editorsOpenerClassNameParameterValue
	^#CMAPEditorsOpener!

homeNameParameterName
	^#homeName!

homeNameParameterValue
	^'CMAPwareHome' copy!

infoHolderClassNameParameterName
	^#infoHolderClassName!

infoHolderClassNameParameterValue
	^#CMAPInfoHolder!

infoStoreMethodSelectorParameterName
	^#infoStoreMethodSelector!

infoStoreMethodSelectorParameterValue
	^#infoStoreMethodSelector01!

metaInfoHolderClassNameParameterName
	^#metaInfoHolderClassName!

metaInfoHolderClassNameParameterValue
	^#CMAPMetaInfoHolder!

metaInfoStoreMethodSelectorParameterName
	^#metaInfoStoreMethodSelector!

metaInfoStoreMethodSelectorParameterValue
	^#metaInfoStoreMethodSelector01!

modelKindLabelParameterName
	^#modelKindLabel!

modelKindLabelParameterValue
	^'ModelKind' copy!

projectClassNameParameterName
	^#projectClassName!

projectClassNameParameterValue
	^#CMAPProject!

rootNameAttributeNameParameterName
	^#rootNameAttributeName!

rootNameAttributeNameParameterValue
	^'cmapWareRootName' copy!

rootNameParameterName
	^#rootName!

rootNameParameterValue
	^'CMAPwareRoot' copy!

savePostfixParameterName
	^#savePostfix!

savePostfixParameterValue
	^'.cmap' copy!

translationHolderClassNameParameterName
	^#translationHolderClassName!

translationHolderClassNameParameterValue
	^#CMAPTranslationHolder!

translationStoreMethodSelectorParameterName
	^#translationStoreMethodSelector!

translationStoreMethodSelectorParameterValue
	^#translationStoreMethodSelector01! !

!CMAPApplicationConfiguration class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsCollectionClass
	^CMAPApplicationConfigurationsCollection! !

!CMAPApplicationConfiguration class publicMethodsFor: 'thoughts'!

thoughts_configurationsNeeded
"
For each application there is a configuration composed of :
An instance of CMAPConfigurationsCollection holding instances of configs below:
An instance of CMAPMetaConfiguration, first initialized with default parameter values, hold separately in memory for each application, and later persisting  parameter values
An instance of CMAPDeveloperConfiguration, first initialized with default parameter values, hold separately in memory for each application, and later persisting parameter values
An instance of CMAPApplicationConfiguration, first initialized with default parameter values, hold separately in memory for each application, and later persisting these parameters
	for CMAP_UI classes
		for CMAPDefinitionsHolder 
			preferredApplicationBrowserClass  
			preferredPathFinderApplicationBrowserClass

		for CMAPInfoHolder
			infoStoreMethodSelector
			rootHomeName
			rootHomeName
			metaInfoStoreClass
			metaInfoStoreMethodSelector
			
		for CMAPProject
			infoStoreClass
			infoStoreMethodSelector
			savePostfix 
			modelKindLabel
			defaultProjectName


"! !

!CMAPApplicationConfiguration publicMethodsFor: 'custom parameter access'!

applicationName
	^self getParameter: self class applicationNameParameterName!

browserClass
	| aBrowserClass aBrowserClassName |
	aBrowserClassName := self getParameter: self class browserClassNameParameterName.
	aBrowserClassName isNil ifTrue: [^nil].
	aBrowserClass := Smalltalk at: aBrowserClassName asSymbol ifAbsent: [ nil].
	aBrowserClass isNil ifTrue: [ ^nil].
	aBrowserClass isBehavior ifFalse: [ ^nil].
	^aBrowserClass!

definitionsHolderFactoryClass
	| aDefinitionsHolderFactoryClassClass aDefinitionsHolderFactoryClassClassName |
	aDefinitionsHolderFactoryClassClassName := self getParameter: self class definitionsHolderFactoryClassNameParameterName.
	aDefinitionsHolderFactoryClassClassName isNil ifTrue: [^nil].
	aDefinitionsHolderFactoryClassClass := Smalltalk at: aDefinitionsHolderFactoryClassClassName asSymbol ifAbsent: [ nil].
	aDefinitionsHolderFactoryClassClass isNil ifTrue: [ ^nil].
	aDefinitionsHolderFactoryClassClass isBehavior ifFalse: [ ^nil].
	^aDefinitionsHolderFactoryClassClass!

developerConfiguration
	| aConfiguration |
	aConfiguration := self getParameter: self class developerConfigurationParameterName.
self halt.
	aConfiguration isNil ifTrue: [ ^nil].

	^aConfiguration!

domainFactoryMethodSelector
	| aMethodSelector |
	aMethodSelector := self getParameter: self class domainFactoryMethodSelectorParameterName.
	^aMethodSelector isNil ifTrue: [ nil] ifFalse: [ aMethodSelector asSymbol]!

editorsOpenerClass
	| aEditorsOpenerClass aEditorsOpenerClassName |
	aEditorsOpenerClassName := self getParameter: self class editorsOpenerClassNameParameterName.
	aEditorsOpenerClassName isNil ifTrue: [^nil].
	aEditorsOpenerClass := Smalltalk at: aEditorsOpenerClassName asSymbol ifAbsent: [ nil].
	aEditorsOpenerClass isNil ifTrue: [ ^nil].
	aEditorsOpenerClass isBehavior ifFalse: [ ^nil].
	^aEditorsOpenerClass!

infoHolderClass
	| aInfoHolderClassName aInfoHolderClass |
	aInfoHolderClassName := self getParameter: self class infoHolderClassNameParameterName.
	aInfoHolderClassName isNil ifTrue: [^nil].
	aInfoHolderClass := Smalltalk at: aInfoHolderClassName asSymbol ifAbsent: [ nil].
	aInfoHolderClass isNil ifTrue: [ ^nil].
	aInfoHolderClass isBehavior ifFalse: [ ^nil].
	^aInfoHolderClass!

infoStoreMethodSelector
	| aInfoStoreMethodSelector |
	aInfoStoreMethodSelector := self getParameter: self class infoStoreMethodSelectorParameterName.
	^aInfoStoreMethodSelector isNil ifTrue: [ nil] ifFalse: [ aInfoStoreMethodSelector asSymbol]!

metaInfoHolderClass
	| aMetaInfoHolderClassName aMetaInfoHolderClass |
	aMetaInfoHolderClassName := self getParameter: self class metaInfoHolderClassNameParameterName.
	aMetaInfoHolderClassName isNil ifTrue: [^nil].
	aMetaInfoHolderClass := Smalltalk at: aMetaInfoHolderClassName asSymbol ifAbsent: [ nil].
	aMetaInfoHolderClass isNil ifTrue: [ ^nil].
	aMetaInfoHolderClass isBehavior ifFalse: [ ^nil].
	^aMetaInfoHolderClass!

metaInfoStoreMethodSelector
	| aMetaInfoStoreMethodSelector |
	aMetaInfoStoreMethodSelector := self getParameter: self class metaInfoStoreMethodSelectorParameterName.
	^aMetaInfoStoreMethodSelector isNil ifTrue: [ nil] ifFalse: [ aMetaInfoStoreMethodSelector asSymbol]!

projectClass
	| aProjectClassName aProjectClass |
	aProjectClassName := self getParameter: self class projectClassNameParameterName.
	aProjectClassName isNil ifTrue: [^nil].
	aProjectClass := Smalltalk at: aProjectClassName asSymbol ifAbsent: [ nil].
	aProjectClass isNil ifTrue: [ ^nil].
	aProjectClass isBehavior ifFalse: [ ^nil].
	^aProjectClass!

translationHolderClass
	| aTranslationHolderClassName aTranslationHolderClass |
	aTranslationHolderClassName := self getParameter: self class translationHolderClassNameParameterName.
	aTranslationHolderClassName isNil ifTrue: [^nil].
	aTranslationHolderClass := Smalltalk at: aTranslationHolderClassName asSymbol ifAbsent: [ nil].
	aTranslationHolderClass isNil ifTrue: [ ^nil].
	aTranslationHolderClass isBehavior ifFalse: [ ^nil].
	^aTranslationHolderClass!

translationStoreMethodSelector
	| aMetaInfoStoreMethodSelector |
	aMetaInfoStoreMethodSelector := self getParameter: self class metaInfoStoreMethodSelectorParameterName.
	^aMetaInfoStoreMethodSelector isNil ifTrue: [ nil] ifFalse: [ aMetaInfoStoreMethodSelector asSymbol]! !

!CMAPApplicationConfigurationsCollection class publicMethodsFor: 'class initialization'!

initialize
	"CMAPApplicationConfigurationsCollection initialize"! !

!CMAPApplicationConfigurationsCollection class publicMethodsFor: 'examples'!

exampleApplicationConfigurationsCollection01

	| aNewApplicationConfigurationsCollection |
	aNewApplicationConfigurationsCollection := self new.
	aNewApplicationConfigurationsCollection 
		addConfiguration:  CMAPApplicationConfiguration exampleApplicationConfiguration01.
	^aNewApplicationConfigurationsCollection!

exampleGenericApplicationConfigurationsCollection01

	| aNewApplicationConfigurationsCollection |
	aNewApplicationConfigurationsCollection := self new.
	aNewApplicationConfigurationsCollection 
		addConfiguration:  CMAPApplicationConfiguration exampleGenericApplicationConfiguration01.
	^aNewApplicationConfigurationsCollection! !

!CMAPApplicationConfigurationsCollection class publicMethodsFor: 'ref:configurations'!

allCurrentConfigurations


	^Array new! !

!CMAPApplicationConfigurationsCollection publicMethodsFor: 'ref:accessing'!

name
	^'Configuraciones de apllicaciones CMAP' copy! !

!CMAPApplicationConfigurationsCollection publicMethodsFor: 'svce'!

applicationConfigurationForApplicationName: theApplicationName

	| aApplicationName someConfigurations aApplicationConfiguration |
	(theApplicationName isNil or: [ theApplicationName isEmpty]) ifTrue: [ ^nil].

	aApplicationName := theApplicationName trimBlanks asUppercase.
	aApplicationName isEmpty ifTrue:  [ ^nil].

	someConfigurations := self configurations.
	(someConfigurations isNil or: [ someConfigurations isEmpty]) ifTrue: [ ^nil].
	
	aApplicationConfiguration := someConfigurations detect: [:aApplicationConfig | | aConfigApplicationName |
		aConfigApplicationName := aApplicationConfig applicationName.
		aConfigApplicationName isNil not and: [ aConfigApplicationName = aApplicationName]
	] ifNone: [ nil].
	^aApplicationConfiguration!

applicationNames

	|  someConfigurations someApplicationNames |

	someConfigurations := self configurations.
	(someConfigurations isNil or: [ someConfigurations isEmpty]) ifTrue: [ ^nil].
	
	someApplicationNames := OrderedCollection new: someConfigurations size.
	someConfigurations do: [:aApplicationConfig | | aApplicationName |
		aApplicationName := aApplicationConfig applicationName.
		(aApplicationName isNil not and: [ aApplicationName isEmpty not]) ifTrue: [ 
			someApplicationNames add: aApplicationName asUppercase
		]
	].

	^someApplicationNames! !

!CMAPApplicationSession class publicMethodsFor: 'instance creation'!

newForUserSession: theUserSession withApplicationConfiguration: theApplicationConfiguration

	| aApplicationSession |
	theUserSession isNil ifTrue: [ ^nil].
	theApplicationConfiguration isNil ifTrue: [ ^nil].

	aApplicationSession := self new.
	aApplicationSession forUserSession: theUserSession withApplicationConfiguration: theApplicationConfiguration.
	^aApplicationSession! !

!CMAPApplicationSession publicMethodsFor: 'configurations'!

applicationConfiguration
	^applicationConfiguration! !

!CMAPApplicationSession publicMethodsFor: 'initialize-release'!

forUserSession: theUserSession withApplicationConfiguration: theApplicationConfiguration

	
	userSession := theUserSession.
	applicationConfiguration := theApplicationConfiguration.!

ui: theUI
	ui := theUI! !

!CMAPApplicationSession publicMethodsFor: 'sessions'!

userSession
	^userSession! !

!CMAPApplicationSession publicMethodsFor: 'svce'!

aboutToCloseProject
	self closeAllEditors!

aboutToCloseService
	self closeAllEditors!

close

	self closeAllowed ifFalse: [ ^false].

	self closeUnconditionally.
	^true!

closeAllowed

	| aService  |

	aService := self service.
	aService isNil ifTrue: [ ^true].
	
	^aService closeAllowed!

closeUnconditionally

	| aService aUserSession |

	aUserSession := self userSession.
	aUserSession isNil ifFalse: [ 
		aUserSession aboutToClose: self
	].

	aService := self service.
	aService isNil ifFalse: [
		aService closeUnconditionally
	].

	self release.!

initService
	service := CMAPService newForApplicationSession: self!

projectHasBeenOpened!

service
	service isNil ifTrue: [ self initService].
	^service! !

!CMAPApplicationSession publicMethodsFor: 'ui'!

ui
	^ui!

uiChooseFile: theFileDialogInterface

	| aUI |
	aUI := self ui.
	aUI isNil ifTrue: [ ^nil].

	^aUI uiChooseFile: theFileDialogInterface!

uiConfirm: theQuestion

	| aUI |
	aUI := self ui.
	aUI isNil ifTrue: [ ^false].

	^aUI uiConfirm: theQuestion!

uiConfirm: theQuestion initialAnswer: theAnswer

	| aUI |
	aUI := self ui.
	aUI isNil ifTrue: [ ^false].

	^aUI uiConfirm: theQuestion initialAnswer: theAnswer!

uiCursor: theCursor showWhile: theBlock

	| aUI |
	theBlock isNil ifTrue: [ ^nil].

	aUI := self ui.
	aUI isNil ifTrue: [ ^theBlock value].

	^aUI uiCursor: theCursor showWhile: theBlock!

uiMessage: theMessage

	| aUI |
	aUI := self ui.
	aUI isNil ifTrue: [ ^nil].

	^aUI uiMessage: theMessage!

uiWarn: theWarning

	| aUI |
	aUI := self ui.
	aUI isNil ifTrue: [ ^nil].

	^aUI uiWarn: theWarning!

uiWarnAndMessage: theMessage

	| aUI |
	aUI := self ui.
	aUI isNil ifTrue: [ ^nil].

	^aUI uiWarnAndMessage: theMessage! !

!CMAPConfigurationsBrowser class publicMethodsFor: 'class initialization'!

initialize
	"CMAPConfigurationsBrowser initialize.
	CMAPConfigurationsBrowser allSubclasses do: [:aClass | aClass initialize]"

	super initialize! !

!CMAPConfigurationsBrowser class publicMethodsFor: 'interface specs'!

windowSpec


	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min: #(#Point 400 300 ) #bounds: #(#Rectangle 32 32 740 680) #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!CMAPConfigurationsBrowser class publicMethodsFor: 'ref:class accessing'!

browserKind
	^#CMAPConfigurationsBrowser! !

!CMAPConfigurationsBrowser class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsCollectionClass
	^CMAPConfigurationsCollection!

preferredEditorsOpenerClass
	^CMAPEditorsOpener! !

!CMAPConfigurationsBrowser publicMethodsFor: 'updating'!

updateWindowsList! !

!CMAPConfigurationsCollection class publicMethodsFor: 'class initialization'!

initialize
	"CMAPConfigurationsCollection initialize"

	super initialize! !

!CMAPConfigurationsCollection class publicMethodsFor: 'ref:configurations'!

allCurrentConfigurations
	^Array 
		with: CMAPMETAConfiguration current
		with: CMAPDeveloperConfiguration current! !

!CMAPConfigurationsCollection publicMethodsFor: 'ref:accessing'!

name
	^'Todos los valores configurables' copy! !

!CMAPDefinitionsHolder class publicMethodsFor: 'instance creation'!

fromModel: theModel
	self shouldNotImplement! !

!CMAPDefinitionsHolder class publicMethodsFor: 'preferences-refin'!

preferredApplicationBrowserClass
	^CMAPApplicationBrowser!

preferredPathFinderApplicationBrowserClass
	^CMAPPathFinder! !

!CMAPDefinitionsHolder class publicMethodsFor: 'specs building'!

buildClassSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildClassSpecFromRelationship: theRelationship
	self shouldNotImplement!

buildCollectionSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildCollectionSpecFromRelationship: theRelationship
	self shouldNotImplement!

buildMetaInfoSpec
	self shouldNotImplement!

buildOrderedCollectionSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildOrderedCollectionSpecFromRelationship: theRelationship
	self shouldNotImplement!

buildPathsFromSpecs: theAllSpecs 
	self shouldNotImplement!

buildPerspectivesFromSpecs: theAllSpecs
	self shouldNotImplement!

buildSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildSpecFromOperation: theOperation
	self shouldNotImplement!

buildSpecFromRelationship: theRelationship
	self shouldNotImplement!

buildTerminalCollectionSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildTerminalOrderedCollectionSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildTerminalSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildTextSpecFromAttribute: theAttribute
	self shouldNotImplement!

buildViewFromSpecs: theAllSpecs
	self shouldNotImplement!

buildViews: theViews perspectives: thePerspectives paths: thePaths fromModel: theModel
	self shouldNotImplement!

buildViews: theViews perspectives: thePerspectives paths: thePaths fromModule: theModule
	self shouldNotImplement!

buildViews: theViews perspectives: thePerspectives paths: thePaths fromType: theType
	self shouldNotImplement!

canChangeFeature: theFeature
	self shouldNotImplement!

classifySpecs: theAllSpecs intoTerminals: theTerminalEditorSpecs separate: theSeparatePerspectiveSpecs
	self shouldNotImplement! !

!CMAPDefinitionsHolderFactory class publicMethodsFor: 'instance creation'!

newWithManager: theManager

	| aDefinitionsHolderFactory |
	theManager isNil ifTrue: [ ^nil].

	aDefinitionsHolderFactory := self new.
	aDefinitionsHolderFactory manager: theManager.

	^aDefinitionsHolderFactory! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'accessing'!

manager
	^manager!

manager: theManager
	manager := theManager! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'configuration access'!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil 
		ifTrue: [ nil] 
		ifFalse: [ aConfig getParameter: aConfig class allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterName].

	aValue isNil ifFalse: [^aValue].
	^false!

applicationConfiguration
	| aManager |
	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].
	^aManager applicationConfiguration!

browseIsAggregateRelationshipsParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseIsAggregateRelationshipsParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseMetaInfoParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseMetaInfoParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseRequiredFeaturesParentAttributeParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseRequiredFeaturesParentAttributeParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseRequiredReferencedFeatureCandidatesParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseRequiredReferencedFeatureCandidatesParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseSeparatePerspectivesForReferencedRequiredFeaturesParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseSeparatePerspectivesForReferencedRequiredFeaturesParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

browseToDomainParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class browseToDomainParameterName].
	aValue isNil ifFalse: [^aValue].
	^false!

developerConfiguration
	^CMAPDeveloperConfiguration current!

maxNumberOfSpecsInEachCreationDialogPerspectiveParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class maxNumberOfSpecsInEachCreationDialogPerspectiveParameterName].
	aValue isNil ifFalse: [^aValue].
	^10!

maxNumberOfSpecsInEachPerspectiveParmValueFromConfig: theDeveloperConfiguration
	| aValue aConfig |

	aConfig := theDeveloperConfiguration.
	aValue := aConfig isNil ifTrue: [ nil] ifFalse: [ aConfig getParameter: aConfig class maxNumberOfSpecsInEachPerspectiveParameterName].
	aValue isNil ifFalse: [^aValue].
	^10! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'constants'!

generalPerspectiveName
	^'General' copy! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'constants-styles'!

styleSelectorBeforeChildren
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#beforeChildren
"*VIPVersion 23-11-95 | 11:52:25 am 'ACV'*"!

styleSelectorBrokenLines
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#brokenLines
"*VIPVersion 23-11-95 | 11:52:28 am 'ACV'*"!

styleSelectorCenter
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#center
"*VIPVersion 23-11-95 | 11:52:30 am 'ACV'*"!

styleSelectorDeltaHeight
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#deltaHeight:
"*VIPVersion 23-11-95 | 11:52:33 am 'ACV'*"!

styleSelectorDeltaWidthChildren
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#deltaWidthChildren:
"*VIPVersion 23-11-95 | 11:52:35 am 'ACV'*"!

styleSelectorDeltaWidthCousin
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#deltaWidthCousin:
"*VIPVersion 23-11-95 | 11:52:37 am 'ACV'*"!

styleSelectorHorizontal
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#horizontal
"*VIPVersion 23-11-95 | 11:52:40 am 'ACV'*"!

styleSelectorNoLines
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#noLines
"*VIPVersion 23-11-95 | 11:52:42 am 'ACV'*"!

styleSelectorPostorder
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#postorder
"*VIPVersion 23-11-95 | 11:52:44 am 'ACV'*"!

styleSelectorPreorder
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#preorder
"*VIPVersion 23-11-95 | 11:52:46 am 'ACV'*"!

styleSelectorStraightLines
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#straightLines
"*VIPVersion 23-11-95 | 11:52:50 am 'ACV'*"!

styleSelectorVertical
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^#vertical
"*VIPVersion 23-11-95 | 11:52:52 am 'ACV'*"! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'example'!

example
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METADefinitionsHolder example."

	^self views: 				nil
		applicationModels: 	nil
		listMenus:			nil
		styleModifiers:		nil
"*VIPVersion 23-11-95 | 11:51:43 am 'ACV'*"!

exampleCustomApplicationModels
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METADefinitionsHolder exampleCustomApplicationModels."


	|  someApplicationModels |
	someApplicationModels := 	Dictionary new
		at: METATestModel put: (Array with: METATestModelAppModel new with: nil); 
		at: METATestClass put: (Array with: METATestClassYetAnotherAppModel new 
			with: #windowSpec); 
		at: METATestAuthor put: (Array with: METATestAuthorAppModel new with: nil); 
		yourself.

	^self views: nil
		applicationModels: someApplicationModels
		listMenus:			nil
		styleModifiers:		nil
"*VIPVersion 23-11-95 | 11:51:49 am 'ACV'*"!

exampleCustomListMenus
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METAPerspectiveBrowser exampleCustomListMenus."


	|   someListMenus |

	"If a menu makes a removal : note that currently, the dependency removal of the node on the value is done by METAEditorHolderWithList>removeObjectFromCollectionNode:, so ..."


	someListMenus := 	Dictionary new

		at: METATestModel put: (Dictionary new

			at: #classes put: (Array 
				with: "an HWHelpPopUpMenu instance " ((HWHelpPopUpMenu
					labels: 'Find Class\Add Class\Remove Class from Model\Remove Class from List\Class IS Persistent\Class IS NOT Persistent\Crash hard disk' 
						withCRs
					lines: #( 1 4 6)
					values: #(findClass classesLinkCreate classesRemove: removeFromList makePersistent makeNotPersistent crashHardDisk))
					helps: #(
						'Find a Class in the Model'
						'Add a Class to the Model' 
						'Remove selected Class from the Model'
						'Remove selected Class from the List of Classes'
						'Instances of the Class ARE Persistent' 
						'Instances of the Class ARE NOT Persistent'
						'Crash hard disk by parking heads on the boot sector cilynder'))

				with: "a controls specification array " #( 	includeDefaultMenu
						(enableWhenNotSelected performerIsCollectionHolder 
							changeListSelectionWithNodeValueIsResult  )
						(alwaysEnabled performerIsCollectionHolder 
							changeListSelectionWithNodeValueIsResult)
						(enableWhenSelected performerIsCollectionHolder)
						(enableWhenSelected performerIsEditorWithList)
						(enableWhenSelected performerIsCollectionElement)
						(enableWhenSelected performerIsCollectionElement)
						(alwaysDisabled	performerIsSeagate "this is a joke")));
			at: #rootClass put: (Array 
				with: "an HWHelpPopUpMenu instance " ((HWHelpPopUpMenu
					labels: 'Find Class'  withCRs
					lines: #()
					values: #(findClass))
					helps: #(
						'Find a Class in the Hierarchy'))
				with: "a controls specification array " #( 	includeDefaultMenu
						(enableWhenNotSelected performerIsTreeHolder 
							changeListSelectionWithResult  )));
			yourself);
		at: METATestResponsibility put: (Dictionary new
			at: #methods put: (Array 
				with: "an HWHelpPopUpMenu instance " ((HWHelpPopUpMenu
					labels: 'Find Method\Add Method\Remove Method from Responsibility\Remove Method from List\Crash hard disk' 
						withCRs
					lines: #( 1 4)
					values: #(findMethod methodsLinkCreate methodsRemove: removeFromList crashHardDisk))
					helps: #(
						'Find a Method in the Responsibility'
						'Add a Method to the Responsibility' 
						'Remove selected Method from the Responsibility'
						'Remove selected Method from the List of Methods'
						'Crash hard disk by parking heads on the boot sector cilynder'))

				with: "a controls specification array " #( 	includeDefaultMenu
						(enableWhenNotSelected performerIsCollectionHolder 
							changeListSelectionWithNodeValueIsResult  )
						(alwaysEnabled performerIsCollectionHolder 
							changeListSelectionWithNodeValueIsResult)
						(enableWhenSelected performerIsCollectionHolder)
						(enableWhenSelected performerIsEditorWithList)
						(alwaysDisabled	performerIsSeagate "this is a joke")));
			yourself);
		at: METATestClass put: (Dictionary new
			at: #author put: (Array 
				with: "an HWHelpPopUpMenu instance " ((HWHelpPopUpMenu
					labels: 'Inspect' 
						withCRs
					lines: #()
					values: #(inspect))
					helps: #(
						'Inspect Author'))

				with: "a controls specification array " #( 	includeDefaultMenu
						(enableWhenSelected performerIsElement nil true )));
			yourself);
		yourself.

	^self views: nil
		applicationModels: nil
		listMenus: someListMenus
		styleModifiers:		nil
"*VIPVersion 26-5-96 | 2:12:01 pm 'Anonymous'*"!

exampleCustomStyles
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METADefinitionsHolder exampleCustomStyles."


	|   someStyleModifiers |
	someStyleModifiers := (Array
		with: self styleSelectorBeforeChildren
		with: self styleSelectorVertical
		with: self styleSelectorStraightLines
		with: self styleSelectorCenter)", 
		(Array with: (Array with: self styleSelectorDeltaHeight with: 40))".


	^self views: nil
		applicationModels: nil
		listMenus:	nil
		styleModifiers: someStyleModifiers

"*VIPVersion 23-11-95 | 11:52:01 am 'ACV'*"!

exampleCustomViews
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METADefinitionsHolder exampleCustomViews."


	| someViews |
	someViews := 	Dictionary new
		"at: METATestClass put: (self preferredCacheEntryClass new
			metaSelectors: METATestObjectKK metaOtherSelectors);"
		at: METATestClass put: (self preferredCacheEntryClass new
			metaSelectorsSource:  METATestClass; 
			metaSelectorsToReject: #('isPersistent'));
		at: METATestAttribute put: (self preferredCacheEntryClass new
			metaSelectorsSource: METATestAttribute;
			metaSelectorsSelector: #otherMETASelectors);
		yourself.

	^self 
		views: someViews
		applicationModels: nil
		listMenus:	nil
		styleModifiers: nil


"*VIPVersion 23-11-95 | 11:52:08 am 'ACV'*"!

exampleCustomViewsAndApplicationModels
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METADefinitionsHolder exampleCustomViewsAndApplicationModels."


	|  someApplicationModels someViews |
	someViews := 	Dictionary new
		"at: METATestClass put: (self preferredCacheEntryClass new
			metaSelectors: METATestObjectKK metaOtherSelectors);"
		at: METATestClass put: (self preferredCacheEntryClass new
			metaSelectorsSource:  METATestClass; 
			metaSelectorsToReject: #('isPersistent'));
		at: METATestAttribute put: (self preferredCacheEntryClass new
			metaSelectorsSource: METATestAttribute;
			metaSelectorsSelector: #otherMETASelectors);
		yourself.

	someApplicationModels := 	Dictionary new
		at: METATestModel put: (Array with: METATestModelAppModel new with: nil); 
		at: METATestClass put: (Array with: METATestClassYetAnotherAppModel new 
			with: #windowSpec); 
		at: METATestAuthor put: (Array with: METATestAuthorAppModel new with: nil); 
		yourself.

	^self 
		views: someViews
		applicationModels: someApplicationModels
		listMenus:	nil
		styleModifiers: nil

"*VIPVersion 23-11-95 | 11:52:14 am 'ACV'*"!

exampleCustomViewsAndApplicationModelsAndListMenus
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METADefinitionsHolder exampleCustomViewsAndApplicationModelsAndListMenus."


	|  someApplicationModels someViews someListMenus |
	someViews := 	Dictionary new
		"at: METATestClass put: (self preferredCacheEntryClass new
			metaSelectors: METATestObjectKK metaOtherSelectors);"
		at: METATestClass put: (self preferredCacheEntryClass new
			metaSelectorsSource:  METATestClass; 
			metaSelectorsToReject: #('isPersistent'));
		at: METATestAttribute put: (self preferredCacheEntryClass new
			metaSelectorsSource: METATestAttribute;
			metaSelectorsSelector: #otherMETASelectors);
		yourself.

	someApplicationModels := 	Dictionary new
		at: METATestModel put: (Array with: METATestModelAppModel new with: nil); 
		at: METATestClass put: (Array with: METATestClassYetAnotherAppModel new 
			with: #windowSpec); 
		at: METATestAuthor put: (Array with: METATestAuthorAppModel new with: nil); 
		yourself.


	someListMenus := 	Dictionary new

		at: METATestModel put: (Dictionary new

			at: #classes put: (Array 
				with: "an HWHelpPopUpMenu instance " ((HWHelpPopUpMenu
					labels: 'Find Class\Add Class\Remove Class from Model\Remove Class from List\Class IS Persistent\Class IS NOT Persistent\Crash hard disk' 
						withCRs
					lines: #( 1 4 6)
					values: #(findClass classesLinkCreate classesRemove: removeFromList makePersistent makeNotPersistent crashHardDisk))
					helps: #(
						'Find a Class in the Model'
						'Add a Class to the Model' 
						'Remove selected Class from the Model'
						'Remove selected Class from the List of Classes'
						'Instances of the Class ARE Persistent' 
						'Instances of the Class ARE NOT Persistent'
						'Crash hard disk by parking heads on the boot sector cilynder'))

				with: "a controls specification array " #( 	includeDefaultMenu
						(enableWhenNotSelected performerIsCollectionHolder 
							changeListSelectionWithNodeValueIsResult  )
						(alwaysEnabled performerIsCollectionHolder 
							changeListSelectionWithNodeValueIsResult)
						(enableWhenSelected performerIsCollectionHolder)
						(enableWhenSelected performerIsEditorWithList)
						(enableWhenSelected performerIsCollectionElement)
						(enableWhenSelected performerIsCollectionElement)
						(alwaysDisabled	performerIsSeagate "this is a joke")));
			yourself);
		yourself.

	^self 
		views: someViews
		applicationModels: someApplicationModels
		listMenus: someListMenus
		styleModifiers: nil

"*VIPVersion 23-11-95 | 11:52:20 am 'ACV'*"!

exampleFilter

	"METADefinitionsHolder exampleFilter."

	|   someFilters |

	someFilters := 	Dictionary new
		at: METATestModel put: (Dictionary new
			at: #classes put: METASimpleFilter exampleClasses;
			yourself);
		yourself.

	^self views: 							nil
		applicationModels: 				nil
		listMenus:						nil
		styleModifiers:					nil
		perspectives:					nil
		perspectivesApplicationModels:	nil
		paths:							nil
		filters:							someFilters
"*VIPVersion 6-6-96 | 6:14:17 am 'Anonymous'*"! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'examples'!

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

!CMAPDefinitionsHolderFactory publicMethodsFor: 'instance creation'!

forWin95ExplorerLike
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	"METADefinitionsHolder exampleCustomStyles."


	|   someStyleModifiers |
	someStyleModifiers := (Array
		with: self styleSelectorBeforeChildren
		with: self styleSelectorVertical
		with: self styleSelectorStraightLines
		with: self styleSelectorCenter)", 
		(Array with: (Array with: self styleSelectorDeltaHeight with: 40))".


	^self views: nil
		applicationModels: nil
		listMenus:	nil
		styleModifiers: someStyleModifiers

"*VIPVersion 1-6-96 | 9:30:16 pm 'ACV'*"!

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

	^aDefinitionsHolder!

listMenus:							theListMenus

	^self views: 							nil
		applicationModels: 				nil
		listMenus:						theListMenus
		styleModifiers:					nil
		perspectives:					nil
		perspectivesApplicationModels:	nil
		paths:							nil
"*VIPVersion 25-4-96 | 1:28:47 pm 'GBL'*"!

new
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^super new initialize
"*VIPVersion 23-11-95 | 11:51:05 am 'ACV'*"!

views:		theViews

	^self views: 							theViews
		applicationModels: 				nil
		listMenus:						nil
		styleModifiers:					nil
		perspectives:					nil
		perspectivesApplicationModels:	nil
		paths:							nil
		filters:							nil
"*VIPVersion 1-7-96 | 3:17:05 pm 'Anonymous'*"!

views: 					theViews
	applicationModels: 	theApplicationModels
	listMenus:				theListMenus
	styleModifiers:			theStyleModifiers
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^self views: 							theViews
		applicationModels: 				theApplicationModels
		listMenus:						theListMenus
		styleModifiers:					theStyleModifiers
		perspectives:					nil
		perspectivesApplicationModels:	nil
		paths:							nil
		filters:							nil
"*VIPVersion 6-6-96 | 5:58:21 am 'Anonymous'*"!

views: 					theViews
	applicationModels: 	theApplicationModels
	listMenus:				theListMenus
	styleModifiers:			theStyleModifiers
	perspectives:			thePerspectives
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^self views: 							theViews
		applicationModels: 				theApplicationModels
		listMenus:						theListMenus
		styleModifiers:					theStyleModifiers
		perspectives:					thePerspectives
		perspectivesApplicationModels:	nil
		paths:							nil
		filters:							nil
"*VIPVersion 6-6-96 | 5:58:27 am 'Anonymous'*"!

views: 									theViews
	applicationModels: 				theApplicationModels
	listMenus:							theListMenus
	styleModifiers:						theStyleModifiers
	perspectives:						thePerspectives
	perspectivesApplicationModels:	thePerspectivesApplicationModels

	^self views: 							theViews
		applicationModels: 				theApplicationModels
		listMenus:						theListMenus
		styleModifiers:					theStyleModifiers
		perspectives:					thePerspectives
		perspectivesApplicationModels:	thePerspectivesApplicationModels
		paths:							nil
		filters:							nil
"*VIPVersion 6-6-96 | 5:58:34 am 'Anonymous'*"!

views: 									theViews
	applicationModels: 				theApplicationModels
	listMenus:							theListMenus
	styleModifiers:						theStyleModifiers
	perspectives:						thePerspectives
	perspectivesApplicationModels:	thePerspectivesApplicationModels
	paths:								thePaths

	^self views: 								theViews
		applicationModels: 					theApplicationModels
		listMenus:							theListMenus
		styleModifiers:						theStyleModifiers
		perspectives:						thePerspectives
		perspectivesApplicationModels:		thePerspectivesApplicationModels
		paths:								thePaths
		filters:								nil

"*VIPVersion 6-6-96 | 1:58:33 am 'Anonymous'*"!

views: 									theViews
	applicationModels: 				theApplicationModels
	listMenus:							theListMenus
	styleModifiers:						theStyleModifiers
	perspectives:						thePerspectives
	perspectivesApplicationModels:	thePerspectivesApplicationModels
	paths:								thePaths
	filters:								theFilters

	|  aDefinitionsHolder  aDictionary  aPerspectivesApplicationModels aDict |

	aDefinitionsHolder := self new.

	aDefinitionsHolder initialViews: 						theViews.
	aDefinitionsHolder initialApplicationModels:				theApplicationModels.
	aDefinitionsHolder initialListMenus: 					theListMenus.
	aDefinitionsHolder initialStyleModifiers: 				theStyleModifiers.
	aDefinitionsHolder initialPerspectives: 					thePerspectives.
	aDefinitionsHolder initialPerspectivesApplicationModels:	thePerspectivesApplicationModels.
	aDefinitionsHolder initialPaths:						thePaths.
	aDefinitionsHolder initialFilters:						theFilters.

	theViews isNil ifFalse: [
		theViews associationsDo: [:anAssoc |
			aDefinitionsHolder views at: anAssoc key put: anAssoc value]].
	
	theApplicationModels isNil ifFalse: [
		theApplicationModels associationsDo: [:anAssoc |
			aDefinitionsHolder applicationModels at: anAssoc key put: anAssoc value]].

	theListMenus isNil ifFalse: [
		theListMenus associationsDo: [:anAssoc |
			aDictionary := aDefinitionsHolder listMenus at: anAssoc key ifAbsent: [
				aDefinitionsHolder listMenus at: anAssoc key put: Dictionary new].
			anAssoc value associations do: [:otraAssoc |
				aDictionary at: otraAssoc key put: otraAssoc value]]].

	theStyleModifiers isNil ifFalse: [
		aDefinitionsHolder clearStyleModifiers.
		theStyleModifiers do: [:aStyleModifier | aDefinitionsHolder styleModifiers add: aStyleModifier]].
	
	thePerspectives isNil ifFalse: [
		thePerspectives associationsDo: [:anAssoc |
			aDefinitionsHolder perspectives at: anAssoc key put: anAssoc value]].
	
	thePerspectivesApplicationModels isNil ifFalse: [
		aPerspectivesApplicationModels := aDefinitionsHolder perspectivesApplicationModels.
		thePerspectivesApplicationModels associationsDo: [:aClassAndPerspectives |
			aDict := aPerspectivesApplicationModels at: aClassAndPerspectives key ifAbsent: [
				aPerspectivesApplicationModels at: aClassAndPerspectives key put: Dictionary new].
			aClassAndPerspectives value associationsDo: [:anAssoc | 
				aDict  at: anAssoc key put: anAssoc value]]].

	thePaths isNil ifFalse: [
		thePaths associationsDo: [:anAssoc |
			aDefinitionsHolder paths at: anAssoc key put: anAssoc value]].

	theFilters isNil ifFalse: [
		theFilters associationsDo: [:anAssoc |
			aDictionary := aDefinitionsHolder filters at: anAssoc key ifAbsent: [
				aDefinitionsHolder filters at: anAssoc key put: Dictionary new].
			anAssoc value associations do: [:otraAssoc |
				aDictionary at: otraAssoc key put: otraAssoc value]]].
	^aDefinitionsHolder
"*VIPVersion 6-6-96 | 1:48:13 am 'Anonymous'*"! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'navigation'!

metaSelectors

	"METAChildSpecViewEditor openOn: METADefinitionsHolder selector: #metaSelectors target: nil selector: nil."

	"METAChildSpecAutoViewEditor openOn: METADefinitionsHolder selector: #metaSelectors target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 5)
		yourself
"*VIPVersion 25-4-96 | 1:28:49 pm 'GBL'*"! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'ojo class'!

ojoClass
	"
	author: ACV 
	date: 22 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^METAOjo
"*VIPVersion 22-11-95 | 5:20:41 pm 'ACV'*"! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'preferences'!

preferredApplicationBrowserClass
	^CMAPApplicationBrowser!

preferredCacheEntryClass
	

	^self preferredPreferencesClass preferredCacheEntryClass!

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

preferredPathFinderApplicationBrowserClass
	^CMAPPathFinder!

preferredPerspectiveSpecClass
	

	^self preferredPreferencesClass preferredPerspectiveSpecClass!

preferredPreferencesClass

	^CMPreferences!

preferredVirtualChildSpecClass
	

	^self preferredPreferencesClass preferredVirtualChildSpecClass! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'specs building'!

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

	self browseMetaInfoConfigParmValue ifFalse: [ ^nil].


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

	| aValueType aNonVirtualType aDeveloperConfiguration |

	theAttribute isNil ifTrue: [ ^nil].

	aDeveloperConfiguration := theAttribute applicationConfiguration developerConfiguration.
	aDeveloperConfiguration isNil ifTrue: [ ^nil].

	(theAttribute name = CODEElement objectDomainCMGOAttributeName and: [ 
		(self browseToDomainParmValueFromConfig: aDeveloperConfiguration) not
	]) ifTrue: [ ^nil].

	(theAttribute name = CODEElement requiredFeaturesParentAttributeName and: [ 
		(self browseRequiredFeaturesParentAttributeParmValueFromConfig: aDeveloperConfiguration)  not
	]) ifTrue: [ ^nil].


	((theAttribute name endsWith: CODEElement candidatesFeatureNameForRequiredReferencedFeaturePostfix) and: [ 
		 (self browseRequiredReferencedFeatureCandidatesParmValueFromConfig: aDeveloperConfiguration) not
	]) ifTrue: [ ^nil].


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

	| aDeveloperConfiguration |
	theRelationship isNil ifTrue: [ ^nil].

	aDeveloperConfiguration := theRelationship applicationConfiguration developerConfiguration.
	aDeveloperConfiguration isNil ifTrue: [ ^nil].

	(theRelationship isAggregated and: [ 
		(self browseIsAggregateRelationshipsParmValueFromConfig: theRelationship applicationConfiguration developerConfiguration) not
	])  ifTrue: [ ^nil].


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

	^theFeature isChangeable or: [ 
		(theFeature referencedType nonVirtualType isPrimitive or: [ 
			theFeature referencedType nonVirtualType isEnumeration])  and: [ 
			theFeature computationKind = theFeature class computationKindInitializedInConstructor and: [ 
				self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParmValueFromConfig: 
					theFeature applicationConfiguration developerConfiguration
			]
		]
	]!

classifySpecs: theAllSpecs intoTerminals: theTerminalEditorSpecs separate: theSeparatePerspectiveSpecs

	theAllSpecs do: [ :aSpec |  
		aSpec isTerminalChildSpec ifTrue: [ 
			aSpec type = #Text ifTrue: [ theSeparatePerspectiveSpecs add: aSpec].
			 theTerminalEditorSpecs add: aSpec
		].
		aSpec isClassChildSpec ifTrue: [ 
			aSpec showInEditor ifTrue: [ theTerminalEditorSpecs add: aSpec].
			(aSpec isCMDriven not or: [ aSpec metaInfo type name endsWith:  CODEElement requiredFeaturesTypePostfix]) 
				ifFalse: [ theSeparatePerspectiveSpecs add: aSpec]
				ifTrue: [ 
					self browseSeparatePerspectivesForReferencedRequiredFeaturesConfigParmValue ifTrue: [  
						theSeparatePerspectiveSpecs add: aSpec
					]
				]
		].
		aSpec isCollectionChildSpec ifTrue: [ 
			theSeparatePerspectiveSpecs add: aSpec
		].
		aSpec isOperationChildSpec ifTrue: [ 
			 theTerminalEditorSpecs add: aSpec
		].
	].! !

!CMAPDefinitionsHolderFactory publicMethodsFor: 'styles'!

defaultStyleModifiers
	"
	author: ACV 
	date: 23 November 1995 
	Does: 
	Returns: 
	Requires: 
	Side Effects: 
	Precondition: 
	Postcondition: 
	Example of use: 
	"

	^OrderedCollection new
		add: self styleSelectorBeforeChildren;
		add: self styleSelectorHorizontal;
		add: self styleSelectorBrokenLines;
		yourself
"*VIPVersion 23-11-95 | 11:53:45 am 'ACV'*"! !

!CMAPDeveloperConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"CMAPDeveloperConfiguration initialize"

	super initialize! !

!CMAPDeveloperConfiguration class publicMethodsFor: 'examples'!

exampleDeveloperConfiguration01
	^self initialConfiguration copyConfiguration.! !

!CMAPDeveloperConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^('Configuracion.\', 
		'El Desarrollador utiliza parametros en esta configuracion para controlar el comportamiento de CMAPware') copy withCRs!

configurationName
	^'Parametros de Configuracion del Desarrollador' copy!

configurationParameters
 
	^(OrderedCollection new: 32)
		add: self browseIsAggregateRelationshipsParameter;
		add: self browseToDomainParameter;
		add: self browseMetaInfoParameter;
		add: self browseRequiredFeaturesParentAttributeParameter;
		add: self browseRequiredReferencedFeatureCandidatesParameter;
		add: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameter;
		add: self discardPerspectivesInCreationDialogsParameter;
		add: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameter;
		add: self createSomeInfoInNewProjectDomainParameter;
		add: self expandRanliteralInitializationExpressionsParameter;
		add: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameter;
		add: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameter;
		add: self maxNumberOfSpecsInEachPerspectiveParameter;
		add: self softResetHealthCodesParameter;
		yourself! !

!CMAPDeveloperConfiguration class publicMethodsFor: 'ref:parameters'!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameter
	^self preferredParameterBooleanClass
			name: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterName
			label: 'Permitir la Modificacion de Datos obligatoriamente introducidos en las pantallas de creacion.'
			value: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterValue
			isEditable: true
			description: (
				'Cuando True, permite modificar los datos obligatoriamente introducidos en la creacion de cada objeto.\',
				'y que normalmente deberian quedar protegidos de modificacion despues de la creacion\',
				'	Es en el momento de crear la informacion, cuando el Usuario debe poner el mayor cuidado en introducir la informacion correcta.\',
				'	Es entonces cuando la fuente y motivo de la informacion esta presente y/o reciente, y es posible asegurar la mayor fidelidad.\',
				'    Dependiendo del grado de seguridad en la Model, y de proteccion de acceso al sistema por no Usuarios\',
				'	asi como el gradoresponsabilidad y confianza depositado en los usuarios,\',
				'	esta opcion permite controlar si estan permitidas modificaciones a posteriori, sin proceso de autorizacion y verificacion.\',
				'Cuando False, no es posible permite modificar los datos obligatoriamente introducidos en la creacion de cada objeto.',
				'    Esta opcion es mas segura, pues impide la modificacion a posteriori de datos producidos.\',
				'	Puede ser necesario activar esta opcion, en casos seleccionados, donde la modificacion de datos a posteriori\',
				'	esta justificada, autorizada, o adecuadamente supervisada')
				withCRs
			defaultValue: self allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterValue
			verificationBlock: nil
			derivationBlock: nil!

alwaysShowSeparateTextPerspectivesInCreationDialogsParameter
	^self preferredParameterBooleanClass
			name: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameterName
			label: 'Siempre mostrar Perspectiva separada para Textos en Pantallas de Creacion de Datos'
			value: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue
			isEditable: true
			description: (
				'Cuando True, las pantallas de creacion de datos mostraran un menu de perspectivas,\',
				'incluyendo perspectivas para todos los textos largos a introducir en la creacion.\',
				'	El Usuario dispone asi, en cada perspectiva separada para texto, del panel completo de la pantalla de creacion\',
				'	para introducir el texto desado\',
				'	Los navegadores resultan mas complejos por la perspectiva adicional de cada texto,\',
				'Cuando False, todos los datos de creacion aparecen en un mismo panel\',
				'reduciendo el espacio en pantalla disponible para que el usuario introduzca el texto.',
				'    Esta opcion es opcional, y para comodidad de entrada de textos.\',
				'	Los textos pueden tener cualquier longitud, independientemente del espacio disponible en pantalla para su introduccion')
				withCRs
			defaultValue: self alwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseIsAggregateRelationshipsParameter
	^self preferredParameterBooleanClass
			name: self browseIsAggregateRelationshipsParameterName
			label: 'Navegar de contenidos a continente'
			value: self browseIsAggregateRelationshipsParameterValue
			isEditable: true
			description: (
				'Cuando True, permite navegar desde objetos contenidos a su objeto continente.\',
				'     Los navegadores resultan mas complejos por el aumentado numero de perspectivas navegables\',
				'     y la facilidad con que el usuario puede navegar ciclicamente sobre los mismos objetos.\',
				'Cuando False, no es posible navegar desde objetos contenidos a su continente.',
				'     Esta navegacion no es usualmente necesaria, pues el usuario suele estar al tanto del continente, pues acaba de navegar por el.\')
				withCRs
			defaultValue: self browseIsAggregateRelationshipsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseMetaInfoParameter
	^self preferredParameterBooleanClass
			name: self browseMetaInfoParameterName
			label: 'Navegar la MetaInformacion de cada Objeto'
			value: self browseMetaInfoParameterValue
			isEditable: true
			description: (
				'Cuando True, permite navegar desde cada Objeto a su MetaInformacion.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional "MetaInfo",\',
				'     y por que el usuario se ve expuesto a la semantica subyacente del modelo de la aplicacion\',
				'Cuando False, no es posible navegar desde los objetos a su MetaInfo.',
				'     Esta navegacion no es necesaria, ya que el usuario se concentra en manejar informacion - no metainformacion.\')
				withCRs
			defaultValue: self browseMetaInfoParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseRequiredFeaturesParentAttributeParameter
	^self preferredParameterBooleanClass
			name: self browseRequiredFeaturesParentAttributeParameterName
			label: 'Navegar la RequiredFeaturesParentAttribute information en la creacion cada Objeto'
			value: self browseRequiredFeaturesParentAttributeParameterValue
			isEditable: true
			description: (
				'Cuando True, permite visualizar el RequiredFeaturesParentAttribute en los Dialogos de creacion de objetos.\',
				'     Los navegadores resultan mas complejos por el campo y perspectiva adicional llamada RequiredFeaturesParentAttribute,\',
				'Cuando False, no es posible permite visualizar el RequiredFeaturesParentAttribute en los Dialogos de creacion de objetos.',
				'     Esta visualizacion no es necesaria, ya que el objeto visualizado es temporal y de uso interno solamente durante la edicion de los parametros de creacion del nuevo objeto.\')
				withCRs
			defaultValue: self browseRequiredFeaturesParentAttributeParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseRequiredReferencedFeatureCandidatesParameter
	^self preferredParameterBooleanClass
			name: self browseRequiredReferencedFeatureCandidatesParameterName
			label: 'Navegar la RequiredReferencedFeatureCandidates informacion de cada Objeto'
			value: self browseRequiredReferencedFeatureCandidatesParameterValue
			isEditable: true
			description: (
				'Cuando True, permite visualizar los candidatos para nuevas referencias en los Dialogos de creacion de objetos.\',
				'     Los navegadores resultan mas complejos por el campo y perspectiva adicional de los candidatos de cada referencia",\',
				'Cuando False, no es posible visualizar los candidatos para nuevas referencias en los Dialogos de creacion de objetos.',
				'     Esta visualizacion no es necesaria, ya que los candidatos seran mostrados al Usuario cuando solicite la creacion de una referencia.\')
				withCRs
			defaultValue: self browseRequiredReferencedFeatureCandidatesParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseSeparatePerspectivesForReferencedRequiredFeaturesParameter
	^self preferredParameterBooleanClass
			name: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameterName
			label: 'Navegar en perspectiva separada cada ReferencedRequiredFeatures para creacion de cada Objeto'
			value: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameterValue
			isEditable: true
			description: (
				'Cuando True, permite visualizar en perspectiva separada las nuevas referencias en los Dialogos de creacion de objetos.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional de cada referencia.\',
				'Cuando False, no permite visualizar en perspectiva separada las nuevas referencias en los Dialogos de creacion de objetos.',
				'     Esta visualizacion no es necesaria, ya que el objeto seleccionado para nueva referencia muestra su nombre en el campo de editor.\')
				withCRs
			defaultValue: self browseSeparatePerspectivesForReferencedRequiredFeaturesParameterValue
			verificationBlock: nil
			derivationBlock: nil!

browseToDomainParameter
	^self preferredParameterBooleanClass
			name: self browseToDomainParameterName
			label: 'Navegar al Dominio global de la Model'
			value: self browseToDomainParameterValue
			isEditable: true
			description: (
				'Cuando True, permite navegar desde objetos raiz al Dominio de OMGCMAPware para la Model bajo inspeccion.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional "Dominio",\',
				'     por que el usuario tiene acceso a los objetos Origen de todos los tipos raiz del sistema\',
				'     y la facilidad con que el usuario puede navegar ciclicamente de los objetos al Dominio, a los Origenes y otros objetos.\',
				'Cuando False, no es posible navegar desde los objetos al Dominio de OMGCMAPware para la Model bajo inspeccion.',
				'     Esta navegacion no es necesaria, pues el Usuario accede a partes mas especificas de la aplicacion.\')
				withCRs
			defaultValue: self browseToDomainParameterValue
			verificationBlock: nil
			derivationBlock: nil!

createSomeInfoInNewProjectDomainParameter
	^self preferredParameterBooleanClass
			name: self createSomeInfoInNewProjectDomainParameterName
			label: 'Crear infomaciones de ejemplo cuando se cree una nueva Model'
			value: self createSomeInfoInNewProjectDomainParameterValue
			isEditable: true
			description: (
				'Cuando True, al crear una nueva Model, la nueva Model sera rellenada con informaciones de ejemplo y test.\',
				'     Usuarios potenciales o noveles, pueden observar esta informacion de ejemplo, como guia a los contenidos soportados por OMGCMAPware,\',
				'     Esta informacion de ejemplo no es apropiada para el negocio real de la Model.\',
				'Cuando False, la nueva Model se creara sin informaciones de ejemplo.\',
				'	La nueva Model carecera de la informacion de ejemplo no apropiada para negocio real de la Model.\',
				'	El Usuario debera introducir la informacion real correspondiente a su negocio.')
				withCRs
			defaultValue: self createSomeInfoInNewProjectDomainParameterValue
			verificationBlock: nil
			derivationBlock: nil!

discardPerspectivesInCreationDialogsParameter
	^self preferredParameterBooleanClass
			name: self discardPerspectivesInCreationDialogsParameterName
			label: 'Descartar el Menu de Perspectivas en Pantallas de Creacion de Datos'
			value: self discardPerspectivesInCreationDialogsParameterValue
			isEditable: true
			description: (
				'Cuando True, las pantallas de creacion de datos mostraran un menu de perspectivas,\',
				'que permiten al usuario acceder en un panel separado a los textos y datos referenciados (si los hay) en la creacion del Dato.\',
				'     Los navegadores resultan mas complejos por la perspectiva adicional de cada texto,\',
				'Cuando False, todos los datos de creacion aparecen en un mismo panel\',
				'reduciendo el espacio en pantalla disponible para que el usuario introduzca el texto,\',
				'y sin la disponibilidad de panel separado para observar la informacion completa de datos referenciados\',
				'    Esta opcion es opcional, y para comodidad de entrada de textos.\',
				'	Los textos pueden tener cualquier longitud, independientemente del espacio disponible en pantalla para su introduccion')
				withCRs
			defaultValue: self discardPerspectivesInCreationDialogsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

expandRanliteralInitializationExpressionsParameter
	^self preferredParameterBooleanClass
			name: self expandRanliteralInitializationExpressionsParameterName
			label: 'Rellenar datos con un numero aleatorio y texto de ejemplo'
			value: self expandRanliteralInitializationExpressionsParameterValue
			isEditable: true
			description: (
				'Cuando True, al crear nuevos datos, las informaciones requeridas seran rellenadas de antemano con una informacion arbitraria.\',
				'	La informacion se compone de un numero aleatorio de tres cifras, y un texto de ejemplo.\',
				'	Esta opcion permite la creacion rapida de informaciones, durante procesos de validacion o demostracion de OMGCMAPware.\',
				'	Esta informacion de ejemplo no es apropiada para el negocio real de la Model.\',
				'	El Usuario puede seleccionar y borrar el numero y texto de ejemplo, e introducir la informacion deseada.\',
				'Cuando False, al crear nuevos datos, las informaciones requeridas apareceran en blanco,\',
				'listos para que el Usuario introduzca los datos apropiados.\',
				'	En general, los datos arbitrarios no son utiles para el Usuario en el funcionamiento normal de la aplicacion.\',
				'	Ademas, el Usuario se vera importunado por la tarea adicional, de eliminar los datos arbitrarios antes de introdcir los apropiados.')
				withCRs
			defaultValue: self expandRanliteralInitializationExpressionsParameterValue
			verificationBlock: nil
			derivationBlock: nil!

maxNumberOfSpecsInEachCreationDialogPerspectiveParameter
 
	^self preferredParameterNumberBoundedClass
			name: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameterName
			label: 'Numero Maximo de Datos en cada panel de Perspectiva de ventana de Creacion'
			value: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameterValue
			isEditable: true
			description:	'El numero maximo de datos mostrados en el panel de cada Perspectiva en ventanas de creacion'
			defaultValue: self maxNumberOfSpecsInEachCreationDialogPerspectiveParameterValue
			verificationBlock: nil
			derivationBlock: nil
			species: #SmallInteger
			lowerBound: 1
			upperBound: 100!

maxNumberOfSpecsInEachPerspectiveParameter
 
	^self preferredParameterNumberBoundedClass
			name: self maxNumberOfSpecsInEachPerspectiveParameterName
			label: 'Numero Maximo de Datos en cada panel de Perspectiva'
			value: self maxNumberOfSpecsInEachPerspectiveParameterValue
			isEditable: true
			description:	'El numero maximo de datos mostrados en el panel de cada Perspectiva'
			defaultValue: self maxNumberOfSpecsInEachPerspectiveParameterValue
			verificationBlock: nil
			derivationBlock: nil
			species: #SmallInteger
			lowerBound: 1
			upperBound: 100!

softResetHealthCodesParameter
	^self preferredParameterBooleanClass
			name: self softResetHealthCodesParameterName
			label: 'Reinicializar las codificaciones de Enfermedades y Procedimientos con el minimo coste'
			value: self softResetHealthCodesParameterValue
			isEditable: true
			description: (
				'Cuando True, la reinicializacion de codificaciones de Enfermedades y Procedimientos se realizara con minimo coste de tiempo.\',
				'Cuando False, la reinicializacion de codificaciones de Enfermedades y Procedimientos se realizara partiendo desde zero.\')
				withCRs
			defaultValue: self softResetHealthCodesParameterValue
			verificationBlock: nil
			derivationBlock: nil! !

!CMAPDeveloperConfiguration class publicMethodsFor: 'ref:parametersvalues'!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterName
	^#allowModificationOfNonModifiableInitializedInCreationTerminalAttributes!

allowModificationOfNonModifiableInitializedInCreationTerminalAttributesParameterValue
	^false!

alwaysShowSeparateTextPerspectivesInCreationDialogsParameterName
	^#alwaysShowSeparateTextPerspectivesInCreationDialogs!

alwaysShowSeparateTextPerspectivesInCreationDialogsParameterValue
	^true!

browseIsAggregateRelationshipsParameterName
	^#browseIsAggregateRelationships!

browseIsAggregateRelationshipsParameterValue
	^false!

browseMetaInfoParameterName
	^#browseMetaInfo!

browseMetaInfoParameterValue
	^false!

browseRequiredFeaturesParentAttributeParameterName
	^#browseRequiredFeaturesParentAttribute!

browseRequiredFeaturesParentAttributeParameterValue
	^false!

browseRequiredReferencedFeatureCandidatesParameterName
	^#browseRequiredReferencedFeatureCandidates!

browseRequiredReferencedFeatureCandidatesParameterValue
	^false!

browseSeparatePerspectivesForReferencedRequiredFeaturesParameterName
	^#browseSeparatePerspectivesForReferencedRequiredFeatures!

browseSeparatePerspectivesForReferencedRequiredFeaturesParameterValue
	^false!

browseToDomainParameterName
	^#browseToDomain!

browseToDomainParameterValue
	^false!

createSomeInfoInNewProjectDomainParameterName
	^#createSomeInfoInNewProjectDomain!

createSomeInfoInNewProjectDomainParameterValue
	^true!

discardPerspectivesInCreationDialogsParameterName
	^#discardPerspectivesInCreationDialogs!

discardPerspectivesInCreationDialogsParameterValue
	^true!

expandRanliteralInitializationExpressionsParameterName
	^#expandRanliteralInitializationExpressions!

expandRanliteralInitializationExpressionsParameterValue
	^true!

maxNumberOfSpecsInEachCreationDialogPerspectiveParameterName
	^#maxNumberOfSpecsInEachCreationDialogPerspective!

maxNumberOfSpecsInEachCreationDialogPerspectiveParameterValue
	^10!

maxNumberOfSpecsInEachPerspectiveParameterName
	^#maxNumberOfSpecsInEachPerspective!

maxNumberOfSpecsInEachPerspectiveParameterValue
	^10!

softResetHealthCodesParameterName
	^#softResetHealthCodes!

softResetHealthCodesParameterValue
	^false! !

!CMAPDeveloperConfiguration class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsCollectionClass
	^CMAPConfigurationsCollection! !

!CMAPEditorsOpener class publicMethodsFor: 'class accessing'!

editorsOpener
	self shouldNotImplement!

editorsOpenerName
	self shouldNotImplement! !

!CMAPEditorsOpener class publicMethodsFor: 'class initialization'!

deinstallGlobalEditorsOpener
	self shouldNotImplement!

initialize!

installGlobalEditorsOpener
	self shouldNotImplement! !

!CMAPEditorsOpener class publicMethodsFor: 'instance creation'!

newWithManager: theManager

	| aEditorsOpener |
	theManager isNil ifTrue: [ ^nil].

	aEditorsOpener := self new.
	aEditorsOpener manager: theManager.

	^aEditorsOpener! !

!CMAPEditorsOpener publicMethodsFor: 'accessing'!

editorsOpenerName
	| anApplicationConfiguration aModelKindLabel |
	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^#CMAPGlobalEditorsOpener].


	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	^aModelKindLabel asSymbol!

manager
	^manager!

manager: theManager
	self initialize.
	manager := theManager.! !

!CMAPEditorsOpener publicMethodsFor: 'configuration access'!

applicationConfiguration
	| aManager |
	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].
	^aManager applicationConfiguration! !

!CMAPEditorsOpener publicMethodsFor: 'editors'!

closeAllEditorsBut: theObject

	| aResult |
	aResult := super closeAllEditorsBut: theObject.

	self updateAllEditorsWindowsList.
	
	^aResult!

closeAllEditorsOfKind: theKind object: theObject  parameter: theParameter

	| aResult |
	aResult := super closeAllEditorsOfKind: theKind object: theObject  parameter: theParameter.

	self updateAllEditorsWindowsList.
	
	^aResult!

inconditionallyOpenEditorOfKind: theKind object: theObject parameter: theParameter 
	openBlock: theBlock

	| aResult |
	aResult := super inconditionallyOpenEditorOfKind: theKind object: theObject parameter: theParameter 
		openBlock: theBlock.

	self updateAllEditorsWindowsList.
	
	^aResult!

informEditorsHaveBeenClosed

	Cursor normal showWhile: [ Dialog warn: 'Sus ventanas han sido cerradas.']!

releaseEditor: theEditor

	| aResult |
	aResult := super releaseEditor: theEditor.

	self updateAllEditorsWindowsList.
	
	^aResult!

updateAllEditorsWindowsList

	| anEditor |

	self editors do: [:someEditors |
		someEditors do: [:anOPYE |
			anEditor := anOPYE at: 3.
			anEditor isNil ifFalse: [ anEditor updateWindowsList]]].! !

!CMAPInfoHolder class publicMethodsFor: 'browse'!

browseCurrentModel
	"CMAPInfoHolder browseCurrentModel "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeModel anModelSamplesRoot |

	aDomainCMGO := CMAPInfoHolder  currentInfoStoreMethodSelector: #cmapInfoStore.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeModel := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: 'homesCMGO' 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Model'].
	aHomeModel isNil ifTrue: [ ^self].

	anModelSamplesRoot := aHomeModel metaInfo getObject: aHomeModel featureNamedValue: 'homeRootsCMGO' 
		detect: 'name' test: [:anModelName | anModelName = 'Samples'] orCreate: 'Samples'.
	anModelSamplesRoot  isNil ifTrue: [ ^self].

	aMetaInfo := aHomeModel metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := EDOCUIDefinitionsHolder fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::Model')  pathSelectorNames:
			#( 'ownedElements' 'MetaInfo').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::CompositeData') pathSelectorNames:
			#('features' 'constraints' 'supertype' 'subtypes'  'typeOfFlowPorts' 'typeOfProperties' 'typeOfAttributes' 'MetaInfo' 'allFeatures').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::Protocol') pathSelectorNames:
			#('ports' 'nodes' 'abstractTransitions' 'initiator' 'responder' 'ownedElements' 'supertype' 'subtypes'  'usedByProtocolPorts' 'portsUsed' 'MetaInfo' 'allPorts').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::Interface') pathSelectorNames:
			#('ports' 'nodes' 'abstractTransitions' 'initiator' 'responder' 'ownedElements' 'supertype' 'subtypes'  'usedByProtocolPorts' 'portsUsed' 'MetaInfo' 'allPorts').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::ProcessComponent') pathSelectorNames:
			#('ports' 'properties' 'uses' 'nodes' 'abstractTransitions' 'bindings' 'responder' 'ownedElements' 'supertype' 'subtypes' 'usedByComponentUsages' 'bindsToOfContextualBindings'  'portsUsed' 'MetaInfo' 'allPorts' 'allProperties').


		"aDefHldr redefine: (aModel typeNamed: 'Nucleo::Model') perspective: 'Samples' selectorNames:
			#('cif' 'razonSocial' 'nombreComercial' 'direccionOficial' 'direccionFacturaAlquiler' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
"
		aDefHldr
	].

	aBrowserClass := InputState default shiftDown 
		ifTrue: [ CODEMModelGenericBrowser ]
		ifFalse:  [ CMAPPathFinder].
	
	aBrowserClass 
		openForObject: 			anModelSamplesRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 3;
				at: METABrowser windowLabelPrefixSymbol put: 'Model';
				at: METABrowser initialPathParameterSymbol put: #(#('ownedElements' ) );
					yourself)! !

!CMAPInfoHolder class publicMethodsFor: 'current'!

currentInfo
	self shouldNotImplement!

currentMessages
	"self  currentInfo browsePath"
	"self  resetCurrentInfos"
	"currentInfos"
	"(self  currentInfoStoreMethodSelector: self defaultCurrentInfoSelector) browsePath"
	"self  resetCurrentInfoStoreMethodSelector: self defaultCurrentInfoSelector"! !

!CMAPInfoHolder class publicMethodsFor: 'default'!

defaultCurrentInfoSelector
	"self  defaultCurrentInfoSelector "

	self shouldNotImplement! !

!CMAPInfoHolder class publicMethodsFor: 'info creation'!

newDomainWithApplicationConfiguration: theApplicationConfiguration

	| aDomainCMGO aHomeModel aMetaInfoStoreMethodSelector aMetaModel aDomainName aHomeName aRootNameAttributeName aRootName aRoot aMetaInfoHolderClass |
	
	theApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aMetaInfoHolderClass := theApplicationConfiguration metaInfoHolderClass.
	aMetaInfoHolderClass isNil ifTrue: [ ^nil].

	aMetaInfoStoreMethodSelector := theApplicationConfiguration metaInfoStoreMethodSelector.
	aMetaInfoStoreMethodSelector isNil ifTrue: [ ^nil].


	aDomainName := theApplicationConfiguration domainName.
	(aDomainName isNil or: [ aDomainName isEmpty]) ifTrue: [ ^nil].

	aHomeName := theApplicationConfiguration homeName.
	(aHomeName isNil or: [ aHomeName isEmpty]) ifTrue: [ ^nil].

	aRootNameAttributeName := theApplicationConfiguration rootNameAttributeName.
	(aRootNameAttributeName isNil or: [ aRootNameAttributeName isEmpty]) ifTrue: [ ^nil].

	aRootName := theApplicationConfiguration rootName.
	(aRootName isNil or: [ aRootName isEmpty]) ifTrue: [ ^nil].

	aMetaModel := aMetaInfoHolderClass  currentModelStoreMethodSelector: aMetaInfoStoreMethodSelector
		withApplicationConfiguration: theApplicationConfiguration. 
	aMetaModel isNil ifTrue: [ ^nil].


	aDomainCMGO := aMetaModel  createDomainObject.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aDomainCMGO metaInfo setObject: aDomainCMGO 
		featureNamed: CODEElement domainNameCMGODomainAttributeName value:  aDomainName copy.

	aHomeModel := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: CODEElement homesCMGODomainRelationshipName 
		detect: CODEElement homeNameCMGODomainAttributeName test: [:aHName | aHName = aHomeName].
	aHomeModel isNil ifTrue: [ ^aDomainCMGO].

	aRoot := aHomeModel metaInfo getObject: aHomeModel featureNamedValue: CODEElement homeRootsCMGODomainAttributeName 
		detect: aRootNameAttributeName  test: [:anModelName | anModelName = aRootName] orCreate: aRootName.
	aRoot  isNil ifTrue: [ ^aDomainCMGO].

	 ^aDomainCMGO! !

!CMAPInfoHolder class publicMethodsFor: 'persistence'!

loadCurrentInfoModelFrom: theFileName

	self shouldNotImplement!

loadCurrentInfoModelFrom: theFileName withApplicationConfiguration: theApplicationConfiguration

	| aDomainCMGO aModel aInfoStoreMethodSelector someCurrentInfos aHomeModel anModelSamplesRoot aMetaInfoStoreClass aMetaInfoStoreMethodSelector |
	
	(theFileName isNil  or: [ theFileName isEmpty]) ifTrue: [ ^nil].

	theApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aInfoStoreMethodSelector := theApplicationConfiguration infoStoreMethodSelector.
	aInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	aMetaInfoStoreClass := theApplicationConfiguration metaInfoStoreClass.
	aMetaInfoStoreClass isNil ifTrue: [ ^nil].

	aMetaInfoStoreMethodSelector := theApplicationConfiguration metaInfoStoreMethodSelector.
	aMetaInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	someCurrentInfos := self currentInfos.
	someCurrentInfos isNil ifTrue: [ ^nil].

	aModel := aMetaInfoStoreClass   currentModelStoreMethodSelector: aMetaInfoStoreMethodSelector. 
	aModel isNil ifTrue: [ ^nil].

	aDomainCMGO := CMGenericObject bossInFromFileNamed: theFileName.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aDomainCMGO recursiveAttachToMetaInfoInModel: aModel.

	aHomeModel := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: CODEElement homesCMGODomainRelationshipName 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = 'Model'].
	aHomeModel isNil ifTrue: [ ^nil].

	anModelSamplesRoot := aHomeModel metaInfo getObject: aHomeModel featureNamedValue:  CODEElement homeRootsCMGODomainAttributeName 
		detect: 'name' test: [:anModelName | anModelName = 'Samples'] orCreate: 'Samples'.
	anModelSamplesRoot  isNil ifTrue: [ ^nil].

	someCurrentInfos at:  aInfoStoreMethodSelector  put: aDomainCMGO.

	^aDomainCMGO!

saveCurrentInfoModelAs: theFileName

	
	self shouldNotImplement!

saveCurrentInfoModelAs: theFileName withApplicationConfiguration: theApplicationConfiguration

	| aDomainCMGO aModel aInfoStoreMethodSelector aRootHomeName aRootName aRootHome aRoot |
	
	(theFileName isNil  or: [ theFileName isEmpty]) ifTrue: [ ^nil].


	theApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aInfoStoreMethodSelector := theApplicationConfiguration infoStoreMethodSelector.

	(self hasCurrentInfoStoreMethodSelector: aInfoStoreMethodSelector) ifFalse: [ ^nil].

	aRootHomeName := theApplicationConfiguration rootHomeName.
	(aRootHomeName isNil or: [ aRootHomeName isEmpty]) ifTrue: [ ^nil].

	aRootName := theApplicationConfiguration rootName.
	(aRootName isNil or: [ aRootName isEmpty]) ifTrue: [ ^nil].


	aDomainCMGO := self currentInfoStoreMethodSelector: aInfoStoreMethodSelector.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aRootHome := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: CODEElement homesCMGODomainRelationshipName 
		detect: 'homeNameCMGO' test: [:aHomeName | aHomeName = aRootHomeName].
	aRootHome isNil ifTrue: [ ^nil].

	aRoot := aRootHome metaInfo getObject: aRootHome featureNamedValue: CODEElement homeRootsCMGODomainAttributeName 
		detect: 'name' test: [:aHRName | aHRName = aRootName].
	aRoot  isNil ifTrue: [ ^nil].

	aModel := aDomainCMGO metaInfo model.
	aModel isNil ifTrue: [ ^nil].


	CMTransaction firstTransactionDo:	[ 

		[
"Here, code to dettach from sections of model that should not be persisted"

			aDomainCMGO recursiveDetachFromMetaInfo.

			aDomainCMGO bossOutToFileNamed: theFileName

		]
			valueNowOrOnUnwindDo: 
		[
			aDomainCMGO recursiveAttachToMetaInfoInModel: aModel.
		].

		CMTransaction undoLastTransaction

	]! !

!CMAPLauncherPanel class publicMethodsFor: 'interface specs'!

accesoriosUsuarioCanvasSpec
	"UIPainter new openOnClass: self andSelector: #accesoriosUsuarioCanvasSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware Accesorios Usuario' 
			#min: #(#Point 271 221 ) 
			#bounds: #(#Rectangle 486 314 757 535 ) 
			#colors: 
			#(#LookPreferences 
				#setBackgroundColor: #(#ColorValue 8191 8191 5119 ) ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 20 0 -20 1 50 0 ) 
					#model: #openCMAPAppBrowserStartingAtArchivoGeneral 
					#label: 'Navegar Archivo General' ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 70 0 -20 1 100 0 ) 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue #green ) ) 
					#model: #snapshot 
					#label: 'Grabar Estado Actual ...' ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 170 0 -20 1 200 0 ) 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue #red ) ) 
					#model: #quit 
					#label: 'Abandonar CMAPware ...' ) ) ) )!

autorCanvasSpec
	"UIPainter new openOnClass: self andSelector: #autorCanvasSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Autor CMAPWare' 
			#min: #(#Point 223 164 ) 
			#max: #(#Point 461 286 ) 
			#bounds: #(#Rectangle 482 213 768 377 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#LabelSpec 
					#layout: #(#Point 20 110 ) 
					#label: 'All rights reserved' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 60 ) 
					#label: 'of Antonio Carrasco Valero' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 10 ) 
					#label: 'CMAPware is property' ) ) ) )!

autorWindowSpec
	"UIPainter new openOnClass: self andSelector: #autorWindowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware Author' 
			#min: #(#Point 290 299 ) 
			#max: #(#Point 290 299 ) 
			#bounds: #(#Rectangle 406 398 696 697 ) 
			#colors: 
			#(#LookPreferences 
				#setForegroundColor: nil 
				#setBackgroundColor: #(#ColorValue 8191 8191 5119 ) 
				#setSelectionForegroundColor: nil 
				#setSelectionBackgroundColor: nil 
				#setBorderColor: nil ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#LabelSpec 
					#layout: #(#LayoutOrigin 0 0.583333 12 0 ) 
					#label: 'ware' 
					#style: #pixelDefault ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin 0 0.583333 12 0 1 0 ) 
					#label: 'O M G  E D O C' ) 
				#(#SubCanvasSpec 
					#layout: #(#LayoutFrame 10 0 60 0 -10 1 -10 1 ) 
					#flags: 0 
					#majorKey: #CMAPLauncherPanel 
					#minorKey: #autorCanvasSpec ) ) ) )!

configurationsCanvasSpec
	"UIPainter new openOnClass: self andSelector: #configuracionesCanvasSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware Configuraciones' 
			#min: #(#Point 271 221 ) 
			#bounds: #(#Rectangle 486 314 757 535 ) 
			#colors: 
			#(#LookPreferences 
				#setBackgroundColor: #(#ColorValue 8191 8191 5119 ) ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 70 0 -20 1 100 0 ) 
					#model: #openBrowsersConfigurationsBrowser 
					#label: 'Configuracion Navegadores' ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 20 0 -20 1 50 0 ) 
					#model: #openDeveloperConfigurationsBrowser 
					#label: 'Configuracion Desarrollador' ) 
		) ) )!

engineeringUtilsCanvasSpec
	"UIPainter new openOnClass: self andSelector: #engineeringUtilsCanvasSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware Desarrollo' 
			#min: #(#Point 271 221 ) 
			#bounds: #(#Rectangle 609 368 880 589 ) 
			#colors: 
			#(#LookPreferences 
				#setBackgroundColor: #(#ColorValue 8191 8191 5119 ) ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 20 0 -20 1 50 0 ) 
					#model: #openCMAPAppBrowser 
					#label: 'Dominio Completo' ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 70 0 -20 1 100 0 ) 
					#model: #openCMAPModelBrowser 
					#label: 'Modelo del Dominio' ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 120 0 -20 1 150 0 ) 
					#model: #openVisualLauncher 
					#label: 'Lanzador Programacion' ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame 20 0 170 0 -20 1 200 0 ) 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue #red ) ) 
					#model: #resetAllCurrentTranslationsModelsAndInfos 
					#label: 'Borrar Todo' ) ) ) )!

soporteCanvasSpec
	"UIPainter new openOnClass: self andSelector: #soporteCanvasSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'Soporte CMAPWare' 
			#min: #(#Point 286 238 ) 
			#max: #(#Point 461 286 ) 
			#bounds: #(#Rectangle 448 554 734 792 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#LabelSpec 
					#layout: #(#Point 20 103 ) 
					#label: 'Ph#  +34 96 383 6999' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 133 ) 
					#label: '' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 163 ) 
					#label: 'e-mail carrascv@teleline.es' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 193 ) 
					#label: 'Web        www.YourML.com' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 40 ) 
					#label: 'Please, contact the author' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 70 ) 
					#label: 'Antonio Carrasco Valero' ) 
				#(#LabelSpec 
					#layout: #(#Point 20 10 ) 
					#label: 'CMAPware Support' ) ) ) )!

soporteWindowSpec
	"UIPainter new openOnClass: self andSelector: #soporteWindowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware Support' 
			#min: #(#Point 290 299 ) 
			#max: #(#Point 290 299 ) 
			#bounds: #(#Rectangle 406 398 696 697 ) 
			#colors: 
			#(#LookPreferences 
				#setForegroundColor: nil 
				#setBackgroundColor: #(#ColorValue 8191 8191 5119 ) 
				#setSelectionForegroundColor: nil 
				#setSelectionBackgroundColor: nil 
				#setBorderColor: nil ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#LabelSpec 
					#layout: #(#LayoutOrigin 0 0.583333 12 0 ) 
					#label: 'ware' 
					#style: #pixelDefault ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin 0 0.583333 12 0 1 0 ) 
					#label: 'O M G  E D O C' ) 
				#(#SubCanvasSpec 
					#layout: #(#LayoutFrame 10 0 60 0 -10 1 -10 1 ) 
					#flags: 0 
					#majorKey: #CMAPLauncherPanel 
					#minorKey: #soporteCanvasSpec ) ) ) )!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware' 
			#min: #(#Point 350 590 ) 
			#max: #(#Point 450 640 ) 
			#bounds: #(#Rectangle 0 0  350 590 ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#LabelSpec 
					#layout: #(#LayoutOrigin 0 0.583333 12 0 ) 
					#label: 'ware' 
					#style: #pixelDefault ) 
				#(#LabelSpec 
					#layout: #(#AlignmentOrigin 0 0.583333 12 0 1 0 ) 
					#label: 'K R O N O' ) 
				 ) ) )! !

!CMAPLauncherPanel publicMethodsFor: 'actions'!

openBrowsersConfigurationsBrowser
	
	CMAPConfigurationsBrowser open!

openCMAPAppBrowser

	CMAPInfoHolder browseCurrentWholeModel!

openCMAPModelBrowser

	CMAPMetaInfoHolder currentModel browsePath!

openDeveloperConfigurationsBrowser
	
	CMAPConfigurationsBrowser open!

openVisualLauncher
	
	VisualLauncher open!

quit

	(Dialog confirm: ('Desea ABANDONAR CMAPware \',
		'y perder todos los cambios desde la ultima grabacion  ?')withCRs initialAnswer: false)  ifFalse: [ ^self].
	
	(Dialog confirm: ('Realmente desea ABANDONAR CMAPware \',
		'y perder todos los cambios desde la ultima grabacion  ?' ) withCRs initialAnswer: false)  ifFalse: [ ^self].
	

	ObjectMemory quit!

resetAllCurrentTranslationsModelsAndInfos

	(Dialog confirm: ('Desea ELIMINAR toda la informacion en CMAPware\',
		'y devolver el Modelo, Pantallas y traducciones al estado original ?')withCRs initialAnswer: false)  ifFalse: [ ^self].
	
	(Dialog confirm: ('Realmente desea ELIMINAR toda la informacion en CMAPware\',
		'y devolver el Modelo, Pantallas y traducciones al estado original ?' ) withCRs initialAnswer: false)  ifFalse: [ ^self].
	

	TranslationsPersistencyHolder resetAllCurrentTranslationsNoDialog.
	CMAPMetaInfoHolder resetAllCurrentModelsNoDialog.
	CMAPInfoHolder resetAllCurrentInfosNoDialog.!

snapshot
	
	| aPrematureExitBlock aNameRoot aNameTermination aFileSystemMatchString aFileSystemFilePostfixSeparator aMatchTemplate someExistingFileNames anIndex aMaxIndex aNumber aNewIndex aNewFileName aSnapshotName aFilename |
	aPrematureExitBlock := [
		Dialog warn: 'La grabacion de "instantanea" del estado de CMAPware has sido cancelada'.
		^nil
	].

	(Dialog confirm: 
		'Realmente desea Grabar una instantanea del estado actual de CMAPware ?' initialAnswer: false) ifFalse: [ 
		aPrematureExitBlock value
	].

	aNameRoot := 'CMAPware_instantanea_'.
	aNameTermination := 'im'.
	aFileSystemMatchString := '*'.
	aFileSystemFilePostfixSeparator := '.'.

	aMatchTemplate := aNameRoot, aFileSystemMatchString, aFileSystemFilePostfixSeparator, aNameTermination.

	someExistingFileNames := Filename filesMatching: aMatchTemplate.
	anIndex := someExistingFileNames isEmpty
		ifTrue: [ 0]
		ifFalse: [ 
			someExistingFileNames size > 32 ifTrue: [ 
				Dialog warn: ('Usted ya tiene archivadas ', someExistingFileNames size printString, ' instantaneas.\',
					'Recuerde que estas instantaneas ocupan espacio en disco\',
					'La grabacion de instantanea va a continuar verificando y grabando ...') withCRs.
			].
			aMaxIndex := 0.
			someExistingFileNames do: [:aFileName |
				aNumber := Object errorSignal handle: [:anEx | anEx returnWith: 0]
					do: [ Number readFrom: (aFileName copyFrom: aNameRoot size + 1 to: aFileName size) readStream].
				aNumber > aMaxIndex ifTrue: [ aMaxIndex := aNumber].
			].
			aMaxIndex
		].

	aNewIndex := anIndex.
	[
		aNewIndex := aNewIndex + 1.
		aNewFileName := aNameRoot, aNewIndex printString, aFileSystemFilePostfixSeparator, aNameTermination.
		aNewFileName asFilename exists
	] whileTrue.
	
	[
		aSnapshotName := Dialog request: 'Por favor, introduzca un nombre de fichero instantanea a grabar' initialAnswer: aNewFileName.
		(aSnapshotName isNil or: [ aSnapshotName trimBlanks isEmpty]) ifTrue: [  aPrematureExitBlock value].

		aFilename := Object errorSignal 
			handle: [:anEx | anEx returnWith: nil]
			do: [ aSnapshotName asFilename].
		aFilename isNil
			ifTrue: [ 
				(Dialog 
					confirm: 'Ha introducido un nombre de fichero incorrecto.\Desea volver a intentarlo ?' withCRs
					initialAnswer: true) ifFalse: [ aPrematureExitBlock value].
				false
			]
			ifFalse: [
				aFilename exists 
					ifTrue: [ 
						(Dialog 
							confirm: 'Ha introducido un nombre de fichero ya existente.\Desea volver a intentarlo ?' withCRs
							initialAnswer: true) ifFalse: [ aPrematureExitBlock value].
						false
					]
					ifFalse: [ true]
			].
	] whileFalse.


	(Dialog confirm: 
		('CMAPware va a grabar una instantanea de su estado en el fichero \\', aSnapshotName ,
		'\ \Desea realmente grabar la instantanea ?') withCRs initialAnswer: true) ifFalse: [  aPrematureExitBlock value].

	"ObjectMemory snapshot: aNewFileName"


	Dialog warn: 
		('CMAPware ha grabado una instantanea de su estado en el fichero \\', aSnapshotName ,
		'\ \Por favor tome nota de este nombre y la situacion o incidencia observada.\Muchas gracias.') withCRs.! !

!CMAPLoginConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"CMAPLoginConfiguration initialize"! !

!CMAPLoginConfiguration class publicMethodsFor: 'current'!

copyCurrent
		self shouldNotImplement!

current
	self shouldNotImplement!

current: theConfiguration
	self shouldNotImplement! !

!CMAPLoginConfiguration class publicMethodsFor: 'examples'!

exampleGenericLoginConfiguration01

	| aConfiguration someParameters aCopy |

	aConfiguration := self new.

	aConfiguration name: self configurationName.
	aConfiguration description: self configurationDescription.

	someParameters := self configurationParameters.
	someParameters do: [:aParameter | aConfiguration parametersAdd: aParameter].

	aConfiguration recalculateParameters.

	aCopy := aConfiguration copyConfiguration.

	aCopy set: self loginNameParameterName value: 'ACV' copy.
	aCopy set: self loginPasswordParameterName value: 'ACV' copy.
	aCopy set: self userConfigurationsCollectionParameterName value: CMAPUserConfigurationsCollection exampleGenericUserConfigurationsCollection01.

	aCopy recalculateParameters.

	^aCopy!

exampleLoginConfiguration01

	| aConfiguration someParameters aCopy |

	aConfiguration := self new.

	aConfiguration name: self configurationName.
	aConfiguration description: self configurationDescription.

	someParameters := self configurationParameters.
	someParameters do: [:aParameter | aConfiguration parametersAdd: aParameter].

	aConfiguration recalculateParameters.

	aCopy := aConfiguration copyConfiguration.

	aCopy set: self loginNameParameterName value: 'ACV' copy.
	aCopy set: self loginPasswordParameterName value: 'ACV' copy.
	aCopy set: self userConfigurationsCollectionParameterName value: CMAPUserConfigurationsCollection exampleUserConfigurationsCollection01.

	aCopy recalculateParameters.

	^aCopy!

exampleSpecificLoginConfiguration01
	"CMAPLoginConfiguration exampleSpecificLoginConfiguration01"

	| aConfiguration someParameters aCopy aMoreGeneralConfiguration |

	aConfiguration := self new.

	aConfiguration name: self configurationName.
	aConfiguration description: self configurationDescription.

	someParameters := self configurationParameters.
	someParameters do: [:aParameter | aConfiguration parametersAdd: aParameter].

	aConfiguration recalculateParameters.

	aCopy := aConfiguration copyConfiguration.

	aCopy set: self loginNameParameterName value: 'ACV' copy.
	aCopy set: self loginPasswordParameterName value: 'ACV' copy.
	aCopy set: self userConfigurationsCollectionParameterName value: CMAPUserConfigurationsCollection exampleSpecificUserConfigurationsCollection01.

	aCopy recalculateParameters.

	aMoreGeneralConfiguration := self exampleGenericLoginConfiguration01.
self halt.
	aCopy moreGeneralConfigurationsAdd:  aMoreGeneralConfiguration.

	^aCopy! !

!CMAPLoginConfiguration class publicMethodsFor: 'instance creation'!

initialConfiguration
	self shouldNotImplement!

installCurrent
	self shouldNotImplement! !

!CMAPLoginConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^'Login configuration' copy!

configurationName
	^'Login' copy!

configurationParameters
 
	^(OrderedCollection new: 32)
		add: self loginNameParameter;
		add: self loginPasswordParameter;
		add: self userConfigurationsCollectionParameter;
		yourself! !

!CMAPLoginConfiguration class publicMethodsFor: 'ref:parameters'!

loginNameParameter
	^self preferredParameterStringClass
			name: self loginNameParameterName
			label: 'Login name'
			value: self loginNameParameterValue
			isEditable: true
			description: ('Login name') withCRs
			defaultValue: self loginNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

loginPasswordParameter
	^self preferredParameterStringClass
			name: self loginPasswordParameterName
			label: 'Login password'
			value: self loginPasswordParameterValue
			isEditable: true
			description: ('Login password') withCRs
			defaultValue: self loginPasswordParameterValue
			verificationBlock: nil
			derivationBlock: nil!

userConfigurationsCollectionParameter
	^self preferredParameterConfigurationsCollectionClass
			name: self userConfigurationsCollectionParameterName
			label: 'UserConfigurations'
			value: self userConfigurationsCollectionParameterValue
			isEditable: true
			description: 'User Configurations Collection'
			defaultValue: self userConfigurationsCollectionParameterValue
			verificationBlock: nil
			derivationBlock: nil! !

!CMAPLoginConfiguration class publicMethodsFor: 'ref:parametersvalues'!

loginNameParameterName
	^#loginName!

loginNameParameterValue
	^'guest' copy!

loginPasswordParameterName
	^#loginPassword!

loginPasswordParameterValue
	^'guest' copy!

userConfigurationsCollectionParameterName
	^#userConfigurationsCollection!

userConfigurationsCollectionParameterValue
	^CMAPUserConfigurationsCollection new! !

!CMAPLoginConfiguration publicMethodsFor: 'custom parameter access'!

loginName
	^self getParameter: self class loginNameParameterName!

loginPassword
	^self getParameter: self class loginPasswordParameterName! !

!CMAPLoginConfigurationsCollection class publicMethodsFor: 'class initialization'!

initialize
	"CMAPLoginConfigurationsCollection initialize"! !

!CMAPLoginConfigurationsCollection class publicMethodsFor: 'examples'!

exampleLoginConfigurationsCollection01
	"CMAPLoginConfigurationsCollection exampleLoginConfigurationsCollection01"
	| aNewLoginConfigurationsCollection |
	aNewLoginConfigurationsCollection := self new.
	aNewLoginConfigurationsCollection 
		addConfiguration:  CMAPLoginConfiguration exampleLoginConfiguration01.
	^aNewLoginConfigurationsCollection! !

!CMAPLoginConfigurationsCollection class publicMethodsFor: 'ref:configurations'!

allCurrentConfigurations
	^Array new! !

!CMAPLoginConfigurationsCollection publicMethodsFor: 'ref:accessing'!

name
	^'LoginConfigurations' copy! !

!CMAPLoginConfigurationsCollection publicMethodsFor: 'svce'!

loginConfigurationForLoginName: theLoginName

	| aLoginName someConfigurations aLoginConfiguration |
	(theLoginName isNil or: [ theLoginName isEmpty]) ifTrue: [ ^nil].

	aLoginName := theLoginName trimBlanks asUppercase.
	aLoginName isEmpty ifTrue:  [ ^nil].

	someConfigurations := self configurations.
	(someConfigurations isNil or: [ someConfigurations isEmpty]) ifTrue: [ ^nil].
	
	aLoginConfiguration := someConfigurations detect: [:aLoginConfig | | aConfigLoginName |
		aConfigLoginName := aLoginConfig loginName.
		aConfigLoginName isNil not and: [ aConfigLoginName = aLoginName]
	] ifNone: [ nil].
	^aLoginConfiguration!

loginNames

	|  someConfigurations someLoginNames |

	someConfigurations := self configurations.
	(someConfigurations isNil or: [ someConfigurations isEmpty]) ifTrue: [ ^nil].
	
	someLoginNames := OrderedCollection new: someConfigurations size.
	someConfigurations do: [:aLoginConfig | | aLoginName |
		aLoginName := aLoginConfig loginName.
		(aLoginName isNil not and: [ aLoginName isEmpty not]) ifTrue: [ 
			someLoginNames add: aLoginName asUppercase
		]
	].

	^someLoginNames! !

!CMAPLoginSession class publicMethodsFor: 'instance creation'!

newForUserConfigurationsCollection: theUserConfigurationsCollection

	| aLoginSession |
	theUserConfigurationsCollection isNil ifTrue: [ ^nil].

	aLoginSession := self new.
	aLoginSession forUserConfigurationsCollection: theUserConfigurationsCollection.
	^aLoginSession! !

!CMAPLoginSession publicMethodsFor: 'configurations'!

initUserConfigurationsCollection
	userConfigurationsCollection := CMAPUserConfigurationsCollection new!

userConfigurationsCollection
	userConfigurationsCollection isNil ifTrue: [ self initUserConfigurationsCollection].
	^userConfigurationsCollection! !

!CMAPLoginSession publicMethodsFor: 'initialize-release'!

forUserConfigurationsCollection: theUserConfigurationsCollection
	userConfigurationsCollection := theUserConfigurationsCollection! !

!CMAPLoginSession publicMethodsFor: 'sessions'!

addUserSession: theUserSession
	| someUserSessions |
	theUserSession isNil ifTrue: [ ^self].

	someUserSessions := self userSessions.
	(someUserSessions isNil or: [ someUserSessions isEmpty]) ifTrue: [ ^self].

	(someUserSessions includes: theUserSession) ifTrue: [ ^self].
	someUserSessions add: theUserSession.

	self changed: #userSessions!

userSessions
	userSessions isNil ifTrue: [ userSessions := OrderedCollection new: 16].
	^userSessions! !

!CMAPLoginSession publicMethodsFor: 'svce'!

aboutToLogout: theUserSession

	| someUserSessions |
	theUserSession isNil ifTrue: [ ^self].

	someUserSessions := self userSessions.
	(someUserSessions isNil or: [ someUserSessions isEmpty]) ifTrue: [ ^self].

	someUserSessions remove: theUserSession ifAbsent: [ ^self].
	self changed: #userSessions!

loginUserNamed: theUserName

	| aUserConfigurationsCollection aUserConfiguration aUserSession |

	theUserName isNil ifTrue:  [ ^nil].

	aUserConfigurationsCollection := self userConfigurationsCollection.
	aUserConfigurationsCollection isNil ifTrue: [ ^nil].

	aUserConfiguration := aUserConfigurationsCollection userConfigurationForUserName: theUserName.
	aUserConfiguration isNil ifTrue:  [ ^nil].
	
	aUserSession := CMAPUserSession newForLoginSession: self withUserConfiguration: aUserConfiguration.
	aUserSession isNil ifTrue:  [ ^nil].

	self addUserSession: aUserSession.
	^aUserSession!

userNames

	| aUserConfigurationsCollection |

	aUserConfigurationsCollection := self userConfigurationsCollection.
	aUserConfigurationsCollection isNil ifTrue: [ ^nil].

	^aUserConfigurationsCollection userNames.! !

!CMAPManager class publicMethodsFor: 'constants'!

saveDefaultPathFileName
	^'Model_CMAPware.cmap' copy! !

!CMAPManager class publicMethodsFor: 'preferences'!

preferredApplicationConfigurationsCollectionClass

	^CMAPApplicationConfigurationsCollection! !

!CMAPManager class publicMethodsFor: 'snapshot'!

justReturnedFromSnapshot
	^justReturnedFromSnapshot == true!

justReturnedFromSnapshot: theValue
	justReturnedFromSnapshot := theValue == true!

removeDanglingDependents

	CODEElement huntInDependents!

systemToCleanState
	ObjectMemory verboseGlobalCompactingGC.! !

!CMAPManager publicMethodsFor: 'accessing'!

applicationConfiguration
	^applicationConfiguration!

defaultPath
	^defaultPath!

defaultPath: thePath
	| aPath |
	aPath := thePath isNil ifTrue: [ '']  ifFalse: [ thePath].

	defaultPath := aPath.
	self saveDefaultPath!

definitionsHolderFactory
	
	^definitionsHolderFactory!

editorsOpener
	
	^editorsOpener!

isLogged
	^self isApplicationChosen and: [  self logged value == true]!

logged

	^logged isNil ifTrue: [logged := false asValue] ifFalse: [logged]!

logged: aBoolean

	self logged value: aBoolean == true!

project
	^project!

project: theProject
	project := theProject.!

saveDefaultPath
	| aPath aSaveDefaultPathFileName aStream |
	aPath := self defaultPath.
	aPath isNil ifTrue: [ ^self].

	aSaveDefaultPathFileName := self class saveDefaultPathFileName.
	aSaveDefaultPathFileName isNil ifTrue: [ ^self].

	[
		aStream := aSaveDefaultPathFileName asFilename writeStream.
		aStream nextPutAll: aPath; cr
	] valueNowOrOnUnwindDo: [ aStream isNil ifFalse: [ aStream close]]!

userName
	^userName!

userName: theUserName
	userName := theUserName! !

!CMAPManager publicMethodsFor: 'actions'!

closeProject
	| aProject  anApplicationConfiguration aModelKindLabel anApplicationName aPrjName |

	self isLogged ifFalse: [^false]. 

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^false].

	anApplicationName := anApplicationConfiguration applicationName.
	(anApplicationName isNil or: [ anApplicationName isEmpty])  ifTrue: [ ^false].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^false].

	aProject := self project.
	aProject isNil ifTrue: [
		self closeAllEditors.
		^true].

	aPrjName := aProject name.

	(self uiConfirm: 'Realmente desea descartar ' , aModelKindLabel , ' actual en ', anApplicationName , '\' withCRs, 
		aPrjName, ' ?' initialAnswer: false) ifFalse: [^false].

	self closeAllEditors.

	aProject close.

	self project: nil.

	self uiMessage: anApplicationName , ' ' , aModelKindLabel , '  named ', aPrjName, ' discarded.'; cr.

	self class systemToCleanState.

	^true!

login: theUserName
	| aUserName |

	theUserName isNil ifTrue:  [ ^nil].

	aUserName := theUserName trimBlanks.
	aUserName isEmpty ifTrue:  [ ^nil].

	(self isLogged and: [ self userName  = aUserName])  ifTrue: [^aUserName].

	self isLogged ifTrue: [ self logout].

	self userName: aUserName.
	self logged value: true.

	^aUserName!

logout
	
	| anApplicationConfiguration anApplicationName |

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^true].

	anApplicationName := anApplicationConfiguration applicationName.
	(anApplicationName isNil or: [ anApplicationName isEmpty])  ifTrue: [ ^true].

	self isLogged ifFalse: [^true]. 

	self closeProject ifFalse: [^false].
	
	self cleanUp.

	self class systemToCleanState.

	^true!

newProject
	| aProject aPath anApplicationConfiguration aProjectClass anApplicationName aModelKindLabel aMessage |


	self isLogged ifFalse: [^nil]. 

	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	anApplicationName := anApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].


	aProjectClass := anApplicationConfiguration projectClass.
	aProjectClass isNil ifTrue: [ ^nil].

	self closeProject ifFalse: [^nil].
	
	self uiCursor: Cursor wait showWhile: [ 
		self uiMessage: 'Creating new ' , aModelKindLabel , ' ...';cr; show: '...Please wait a few seconds.';cr.

		aProject := aProjectClass createNewProjectWithApplicationConfiguration: self applicationConfiguration.
		aProject isNil 
			ifTrue: [ 
				aMessage :=  ('Error while creating new ' , aModelKindLabel , '\.', 
					'Please, take a Snapshort (menu option in ' , anApplicationName,  ' >>Snapshot)\',
					'and contact ', anApplicationName , ' service.',
					'Thank you.') withCRs.
				self uiWarnAndMessage: aMessage.
				^nil
			]
			ifFalse: [ 
				aMessage :=  ('New ' , aModelKindLabel , ' Created.\',
					'You may continue working.\',
					'Please note that the ' , aModelKindLabel , ' menu has been enabled.') withCRs.
				self uiWarnAndMessage: aMessage.
			].
	].

	aPath := self defaultPath.
	aPath isNil ifFalse: [ aProject folder: aPath].

	self project: aProject.

	self uiMessage: 'New ' , aModelKindLabel , '  ',  anApplicationName , '  ', aProject name, ' created. '; 
		show: Time now printString; cr.

	^aProject!

openDomainBrowser

	| anApplicationConfiguration aInfoHolderClass aInfoStoreMethodSelector |
	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aInfoHolderClass := anApplicationConfiguration infoHolderClass.
	aInfoHolderClass isNil ifTrue: [ ^nil].

	aInfoStoreMethodSelector := anApplicationConfiguration infoStoreMethodSelector.
	aInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	Cursor wait showWhile: [ 
		aInfoHolderClass browseModelStoreMetodSelector:aInfoStoreMethodSelector withApplicationConfiguration: anApplicationConfiguration
	]!

openMetaModelBrowser

	| anApplicationConfiguration aMetaInfoHolderClass aMetaInfoStoreMethodSelector |
	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aMetaInfoHolderClass := anApplicationConfiguration metaInfoHolderClass.
	aMetaInfoHolderClass isNil ifTrue: [ ^nil].

	aMetaInfoStoreMethodSelector := anApplicationConfiguration metaInfoStoreMethodSelector.
	aMetaInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	Cursor wait showWhile: [ 
		aMetaInfoHolderClass browseMetaModelStoreMetodSelector:aMetaInfoStoreMethodSelector withApplicationConfiguration: anApplicationConfiguration
	]!

openProject
	| aProject aMessage anApplicationConfiguration aProjectClass aModelKindLabel anApplicationName |


	self isLogged ifFalse: [^nil]. 

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^nil].

	anApplicationName := anApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aProjectClass := anApplicationConfiguration projectClass.
	aProjectClass isNil ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].

	aProjectClass := anApplicationConfiguration projectClass.
	aProjectClass isNil ifTrue: [ ^nil].

	self closeProject ifFalse: [^nil].
	
	self uiCursor: Cursor wait showWhile: [ 
		self uiMessage:  'Reading ', aModelKindLabel , '  ...';cr; show: '...Please wait a few seconds';cr.

		aProject := aProjectClass openProjectInFolder: self defaultPath withManager: self.
		aProject == false ifTrue: [
			aMessage :=  'User cancelled read operation'.
			self uiWarnAndMessage: aMessage; cr.
			^nil
		].
		aProject isNil 
			ifTrue: [ 
				aMessage :=  ('Error while reading model.\', 
					'Please, take a Snapshort (menu option in ' , anApplicationName , '>>Snapshot)\',
					'and contact  ' , anApplicationName , ' service.',
					'Thank you.') withCRs.
				self uiWarnAndMessage: aMessage.
				^nil
			]
			ifFalse: [ 
				aMessage :=  (aModelKindLabel , '  read completed.\',
					'You may continue working.\',
					'Please note that the  ', aModelKindLabel , '  menu has been enabled.') withCRs.
				self uiWarnAndMessage: aMessage.
			].
	].


	self project: aProject.

	self uiMessage: anApplicationName , '   ', aModelKindLabel , '  named ', aProject name ,  ' loaded. ';
		show: Date today printString; show: Time now printString;cr.

	^aProject!

quit

	(self uiConfirm: 'Desea ABANDONAR CMAPware ?' withCRs initialAnswer: false)  ifFalse: [ ^self].
	(self uiConfirm: 'Realmente desea ABANDONAR CMAPware ?'  withCRs initialAnswer: false)  ifFalse: [ ^self].

	self closeRequest.
	
	DEBUGDvpt ifFalse: [ ObjectMemory quit]!

resetAllCurrentTranslationsModelsAndInfos

	(self uiConfirm: ('Desea ELIMINAR toda la informacion en CMAPware\',
		'y devolver el Modelo y traducciones al estado original ?')withCRs initialAnswer: false)  ifFalse: [ ^self].
	
	(self uiConfirm: ('Realmente desea ELIMINAR toda la informacion en CMAPware\',
		'y devolver el Modelo y traducciones al estado original ?' ) withCRs initialAnswer: false)  ifFalse: [ ^self].
	

	TranslationsPersistencyHolder resetAllCurrentTranslationsNoDialog.
	CMAPMetaInfoHolder resetAllCurrentModelsNoDialog.
	CMAPInfoHolder resetAllCurrentInfosNoDialog.!

retrieveCurrentModel

	| anApplicationConfiguration aInfoHolderClass |
	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aInfoHolderClass := anApplicationConfiguration infoHolderClass.
	aInfoHolderClass isNil ifTrue: [ ^nil].

	^aInfoHolderClass currentModelWIthApplicationConfiguration: anApplicationConfiguration!

saveAsProject
	| aProject aFolder aName aMessage aSavePostfix aFileDialogInterface aFilename aFilePath aFilePathFilename anApplicationConfiguration aModelKindLabel anApplicationName |

	self isLogged ifFalse: [^nil]. 

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^nil].

	anApplicationName := anApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].

	aSavePostfix := anApplicationConfiguration savePostfix.
	(aSavePostfix isNil or: [ aSavePostfix isEmpty])  ifTrue: [ ^nil].

	aProject := self currentlyOpenedProject.
	aProject isNil ifTrue: [
		self uiWarn: 'No hay abierta ningun ' , aModelKindLabel , ' ' , anApplicationName.
		^nil].

	aFolder := aProject folder.
	aFolder isNil ifTrue: [ aFolder := ''].
	aName := aProject saveFileName.
	aName isNil ifTrue: [ aFolder := 'SinNombre'].

	aFileDialogInterface := FileDialogInterface new.
	aFileDialogInterface isNil ifTrue: [ ^nil].
	aFileDialogInterface titleBarText: 'Grabar  ' , aModelKindLabel, ' ', aName.
	aFileDialogInterface addOption: #overwritePrompt.
	aFileDialogInterface fileFilter: ((OrderedCollection new: 1) 
		add:  (Association key:  'Ficheros de  ' , aModelKindLabel, '  ' , anApplicationName , '  (*', aSavePostfix, ')' value: '*' ,aSavePostfix); 
		add:  (Association key: 'Todos los ficheros (*.*)' value: '*.*'); 
		yourself).
	aFileDialogInterface fileName:  aName.

	(aFolder isNil not and: [ aFolder isEmpty not]) ifTrue: [
		aFilename := aFolder asFilename.
		(aFilename exists and: [aFilename isDirectory]) ifTrue: [
			aFileDialogInterface initialDirectory: aFolder]].


	aFilePath := self uiChooseFile: aFileDialogInterface.
	aFilePath isNil ifTrue: [ ^false].

	aFilePathFilename := aFilePath asFilename.

	aFilePathFilename exists 
		ifTrue: [
			(self uiConfirm: 'Fichero\' withCRs, aFilePath, '\ya existe\' withCRs, 'Desea sobreescribirlo ?\' withCRs,
			' ( nota : sus ventanas se cerraran para facilitar la grabacion )' initialAnswer: true) ifFalse: [ ^nil].
		]
		ifFalse: [
			(self uiConfirm: 'Desea grabar  ' , aModelKindLabel, ' \' withCRs, 'en el directorio\' withCRs,
			aFilePathFilename head, '\en el fichero\' withCRs, aFilePathFilename tail , ' ?\' withCRs,
			' ( nota : sus ventanas se cerraran para facilitar la grabacion )' initialAnswer: true) ifFalse: [ ^nil].
		].

	self closeAllEditors.
	self class removeDanglingDependents.

	(aProject saveAs: aFilePath) isNil 
		ifTrue: [ 
			aMessage :=  'Ha ocurrido un error grabando  ' , aModelKindLabel, '  en el fichero\' withCRs, aName, '\' withCRs,
				'Por favor, tome una instantanea (opcion de menu ' , anApplicationName , ' >>Grabar instantanea)\' withCRs,
				'y contacte el servicio tecnico de ' , anApplicationName , ' .\' withCRs,
				'Gracias\'.
			self uiWarnAndMessage: aMessage.
			^nil
		]
		ifFalse: [ 
			aMessage :=   aModelKindLabel, '  ' , anApplicationName , ' \' withCRs, aProject name , 
				'\ Grabada en Directorio : \' withCRs,aFilePathFilename head, '\en el fichero\' withCRs, aFilePathFilename tail .
			self uiWarnAndMessage: aMessage.
		].!

saveProject
	| aProject aFolder aName aMessage anApplicationConfiguration aModelKindLabel anApplicationName |

	self isLogged ifFalse: [^nil]. 

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].

	anApplicationName := anApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aProject := self currentlyOpenedProject.
	aProject isNil ifTrue: [
		self uiWarn: 'No hay abierta ningun ', aModelKindLabel , ' ' , anApplicationName.
		^nil].

	aFolder := aProject folder.
	aFolder isNil ifTrue: [ aFolder := ''].
	aName := aProject saveFileName.
	aName isNil ifTrue: [ aName := 'SinNombre'].

	(self uiConfirm: ('Realmente desea Grabar  ', aModelKindLabel , ' ' , anApplicationName, ' \' withCRs, aProject name , 
		'\en el Directorio : \' withCRs, aFolder , '\en fichero llamado\' withCRs, aName , ' ?\' withCRs,
		' ( nota : sus ventanas se cerraran para facilitar la grabacion )') ) ifFalse: [^nil].		

	self backupProjectExistingSaveFile: aProject.

	self closeAllEditors.
	self class removeDanglingDependents.

	aProject save isNil 
		ifTrue: [ 
			aMessage :=  'Ha ocurrido un error grabando ', aModelKindLabel , ' en el fichero\' withCRs, aName, '\' withCRs,
				'Por favor, tome una instantanea (opcion de menu  ' , anApplicationName, ' >>Grabar instantanea)\' withCRs,
				'y contacte el servicio tecnico de  ' , anApplicationName, ' .\' withCRs,
				'Gracias\'.
			self uiWarnAndMessage: aMessage.
			^nil
		]
		ifFalse: [ 
			aMessage :=  aModelKindLabel , '  ' , anApplicationName, ' \' withCRs, aProject name , 
				'\ Grabado en Directorio : \' withCRs, aProject folder , '\en fichero llamado\' withCRs, aProject saveFileName.
			self uiWarnAndMessage: aMessage.
		].!

snapshot
	
	| aPrematureExitBlock aNameRoot aNameTermination aFileSystemMatchString aFileSystemFilePostfixSeparator aMatchTemplate someExistingFileNames anIndex aMaxIndex aNumber aNewIndex aNewFileName aSnapshotName aFilename aDotPosition aDate aTime aMessage |
	aPrematureExitBlock := [
		self uiWarn: 'La grabacion de "instantanea" del estado de CMAPware has sido cancelada'.
		^nil
	].

	(self uiConfirm: 
		'Realmente desea Grabar una instantanea del estado actual de CMAPware ?' initialAnswer: false) ifFalse: [ 
		aPrematureExitBlock value
	].

	aNameRoot := 'CMAPware_instantanea_'.
	aNameTermination := 'im'.
	aFileSystemMatchString := '*'.
	aFileSystemFilePostfixSeparator := '.'.

	aMatchTemplate := aNameRoot, aFileSystemMatchString, aFileSystemFilePostfixSeparator, aNameTermination.

	someExistingFileNames := Filename filesMatching: aMatchTemplate.
	anIndex := someExistingFileNames isEmpty
		ifTrue: [ 0]
		ifFalse: [ 
			someExistingFileNames size > 32 ifTrue: [ 
				self uiWarn: ('Usted ya tiene archivadas ', someExistingFileNames size printString, ' instantaneas.\',
					'Recuerde que estas instantaneas ocupan espacio en disco\',
					'La grabacion de instantanea va a continuar verificando y grabando ...') withCRs.
			].
			aMaxIndex := 0.
			someExistingFileNames do: [:aFileName |
				aNumber := Object errorSignal handle: [:anEx | anEx returnWith: 0]
					do: [ Number readFrom: (aFileName copyFrom: aNameRoot size + 1 to: aFileName size) readStream].
				aNumber > aMaxIndex ifTrue: [ aMaxIndex := aNumber].
			].
			aMaxIndex
		].

	aNewIndex := anIndex.
	[
		aNewIndex := aNewIndex + 1.
		aNewFileName := aNameRoot, aNewIndex printString, aFileSystemFilePostfixSeparator, aNameTermination.
		aNewFileName asFilename exists
	] whileTrue.
	
	[
		aSnapshotName := Dialog request: 'Por favor, introduzca un nombre de fichero instantanea a grabar' initialAnswer: aNewFileName.
		(aSnapshotName isNil or: [ aSnapshotName trimBlanks isEmpty]) ifTrue: [  aPrematureExitBlock value].
		aDotPosition := aSnapshotName indexOf: $..
		aDotPosition > 0 ifTrue: [ aSnapshotName := aSnapshotName copyFrom: 1 to: aDotPosition -1].
	
		aFilename := Object errorSignal 
			handle: [:anEx | anEx returnWith: nil]
			do: [ (aSnapshotName , aFileSystemFilePostfixSeparator, aNameTermination) asFilename].
		aFilename isNil
			ifTrue: [ 
				(self uiConfirm: 'Ha introducido un nombre de fichero incorrecto.\Desea volver a intentarlo ?' withCRs
					initialAnswer: true) ifFalse: [ aPrematureExitBlock value].
				false
			]
			ifFalse: [
				aFilename exists 
					ifTrue: [ 
						(self uiConfirm: 'Ha introducido un nombre de fichero ya existente.\Desea volver a intentarlo ?' withCRs
							initialAnswer: true) ifFalse: [ aPrematureExitBlock value].
						false
					]
					ifFalse: [ true]
			].
	] whileFalse.


	(self uiConfirm: 
		('CMAPware va a grabar una instantanea de su estado en el fichero \\', aSnapshotName ,
		'\ \Desea realmente grabar la instantanea ?') withCRs initialAnswer: true) ifFalse: [  aPrematureExitBlock value].

	self class justReturnedFromSnapshot: false.
	aDate := SpanishDate today.
	aTime:= Time now.

	self panelsTabsModel selectionIndex: 1.
	self panelsTabsModel selectionIndex: 2.

	ObjectMemory verboseGlobalCompactingGC.
	ObjectMemory snapshotAs: aSnapshotName thenQuit: false.

	self class justReturnedFromSnapshot
		ifTrue: [ 
			aMessage :=
				('CMAPware ha sido lanzado desde una instantanea de su estado que fue grabada el\',
				aDate printString , '  a las ' , aTime printString , '\en el fichero \\', aSnapshotName ,
				'\Todas las informaciones y ventanas se encuentran exactamente como estaban en ese momento.') withCRs.
			self uiWarn: aMessage.
			self uiMessage: aMessage; cr
		]
		ifFalse:[ 
			aMessage := ('CMAPware ha grabado una instantanea de su estado en el fichero \ \', aSnapshotName ,
				'\ \Por favor tome nota de este nombre y el motivo por el que Usted ha grabado la instantanea\.',
				'Por ejemplo, grabacion del trabajo actual,\',
				'o en caso de una situacion exceptional, por favor tome nota de la relevancia de la situacion o incidencia observada.\',
				'Muchas gracias.') withCRs.
			self uiWarn: aMessage.
			self uiMessage: aMessage; cr
		].

		(Delay forMilliseconds: 500) wait.
		self panelsTabsModel selectionIndex: 2.
		self panelsTabsModel selectionIndex: 1.! !

!CMAPManager publicMethodsFor: 'application'!

candidateApplicationConfigurations

	| someApplicationConfigurations anApplicationConfigurationsCollection |

	anApplicationConfigurationsCollection := self preferredApplicationConfigurationsCollectionClass current.
	anApplicationConfigurationsCollection isNil ifTrue: [ ^nil].

	someApplicationConfigurations := anApplicationConfigurationsCollection configurations.
	(someApplicationConfigurations isNil or: [ someApplicationConfigurations isEmpty]) ifTrue: [ ^nil].

	^someApplicationConfigurations asArray!

isApplicationChosen

	^self applicationConfiguration isNil not!

releaseApplicationConfiguration

	applicationConfiguration isNil ifFalse: [ applicationConfiguration := nil].

	editorsOpener isNil ifFalse: [ 
		editorsOpener release.
		editorsOpener := nil
	].

	definitionsHolderFactory isNil ifFalse: [ 
		definitionsHolderFactory release.
		definitionsHolderFactory := nil
	].!

selectApplicationConfiguration: theApplicationConfiguration

	| aDefinitionsHolderFactoryClass aDefinitionsHolderFactory aEditorsOpenerClass aEditorsOpener aDone |

	(theApplicationConfiguration isNil not and: [ theApplicationConfiguration == self applicationConfiguration])  ifTrue: [ ^self].

	self releaseApplicationConfiguration.
	
	theApplicationConfiguration isNil ifTrue: [ ^self].

	applicationConfiguration := theApplicationConfiguration.

	aDone := false.
	[
		aDefinitionsHolderFactoryClass := theApplicationConfiguration definitionsHolderFactoryClass.
		aDefinitionsHolderFactoryClass isNil ifTrue: [ ^self].

		aDefinitionsHolderFactory := aDefinitionsHolderFactoryClass newWithManager: self.
		aDefinitionsHolderFactory isNil ifTrue: [ ^self].
		definitionsHolderFactory := aDefinitionsHolderFactory.


		aEditorsOpenerClass := theApplicationConfiguration editorsOpenerClass.
		aEditorsOpenerClass isNil ifTrue: [ ^self].

		aEditorsOpener := aEditorsOpenerClass newWithManager: self.
		aEditorsOpener isNil ifTrue: [ ^self].
		editorsOpener := aEditorsOpener.

		aDone := true
	]
		valueNowOrOnUnwindDo: 
	[
		aDone ifFalse: [ 	 
			self releaseApplicationConfiguration
		]
	].

	^self! !

!CMAPManager publicMethodsFor: 'clean'!

cleanUp
	self isLogged ifFalse: [^self]. 

	self closeAllEditors.

	userName := nil.			
	self logged: false.!

closeAllEditors
	| anEditorsOpener |

	anEditorsOpener := self editorsOpener.
	anEditorsOpener isNil ifTrue: [ ^self].

	anEditorsOpener closeAllEditors! !

!CMAPManager publicMethodsFor: 'editors'!

applicationWindowEntered: theEditor
	| anEditorsOpener |
	anEditorsOpener := self editorsOpener.
	anEditorsOpener isNil ifTrue: [ ^self].
	anEditorsOpener applicationWindowEntered: theEditor! !

!CMAPManager publicMethodsFor: 'initialize-release'!

pilot: thePilot
	pilot := thePilot! !

!CMAPManager publicMethodsFor: 'preferences'!

preferredApplicationConfigurationsCollectionClass

	^self class preferredApplicationConfigurationsCollectionClass! !

!CMAPManager publicMethodsFor: 'project'!

backupProjectExistingSaveFile: theProject

	| aProjectName aBackupProjectPostfix aFileSystemMatchString aNameTermination aMatchTemplate someExistingFileNames anIndex aMaxIndex aNumber aNewIndex aNewFileName aProjectFilePath aProjectFolder aMessage anApplicationConfiguration aModelKindLabel |

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].


	theProject isNil ifTrue: [ ^nil].

	aProjectFilePath := theProject saveFilePath.
	(aProjectFilePath isNil or: [ aProjectFilePath isEmpty]) ifTrue: [ ^nil].
	aProjectFilePath asFilename exists ifFalse: [ ^nil].	

	aProjectFolder := theProject folder.
	aProjectName := theProject name.
	(aProjectName isNil or: [ aProjectName isEmpty]) ifTrue: [ ^nil].

	aBackupProjectPostfix :=  theProject  backupPostfix.
	aFileSystemMatchString := '*'.
	aNameTermination := theProject savePostfix.

	aMatchTemplate := aProjectFolder , (String with: Filename separator) , 
		aProjectName, aBackupProjectPostfix , aFileSystemMatchString, aNameTermination.

	someExistingFileNames := Filename filesMatching: aMatchTemplate.
	anIndex := someExistingFileNames isEmpty
		ifTrue: [ 0]
		ifFalse: [ 
			aMaxIndex := 0.
			someExistingFileNames do: [:aFileName |
				aNumber := Object errorSignal handle: [:anEx | anEx returnWith: 0]
					do: [ Number readFrom: (aFileName copyFrom: aProjectName size + 1 to: aFileName size) readStream].
				aNumber > aMaxIndex ifTrue: [ aMaxIndex := aNumber].
			].
			aMaxIndex
		].

	aNewIndex := anIndex.
	[
		aNewIndex := aNewIndex + 1.
		aNewFileName := aProjectFolder , (String with: Filename separator) , 
			aProjectName, aBackupProjectPostfix , aNewIndex printString,  aNameTermination.
		aNewFileName asFilename exists
	] whileTrue.

		
	aProjectFilePath asFilename renameTo: aNewFileName.

	aMessage := 'Una Copia de Seguridad\del archivo de ',  aModelKindLabel , '\' withCRs, aProjectName, '\ha sido guardada en el directorio\' withCRs,
		aProjectFolder, '\en el fichero\' withCRs,
		aProjectName, aBackupProjectPostfix , aNewIndex printString,  aNameTermination.
	self uiWarnAndMessage: aMessage.!

hasOpenedProject
	^project isNil not!

projectRoot

	| aModel  aMetaInfo aBrowserClass aDomainCMGO aHomeModel anApplicationConfiguration aModelKindLabel anApplicationName aProject aHomeName aRoot aRootNameAttributeName aRootName aDefinitionsHolderFactory |
	
	self isLogged ifFalse: [ ^nil].

	aProject := self project.
	aProject isNil ifTrue: [ ^nil].

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^nil].

	aHomeName := anApplicationConfiguration homeName.
	(aHomeName isNil or: [ aHomeName isEmpty]) ifTrue: [ ^nil].

	aRootNameAttributeName := anApplicationConfiguration rootNameAttributeName.
	(aRootNameAttributeName isNil or: [ aRootNameAttributeName isEmpty]) ifTrue: [ ^nil].

	aRootName := anApplicationConfiguration rootName.
	(aRootName isNil or: [ aRootName isEmpty]) ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].

	anApplicationName := anApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aBrowserClass := anApplicationConfiguration browserClass.
	aBrowserClass isNil ifTrue: [ ^nil].


	aDefinitionsHolderFactory := self definitionsHolderFactory.
 	aDefinitionsHolderFactory isNil ifTrue: [ ^nil].

	aDomainCMGO := aProject domainCMGO.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aHomeModel := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: CODEElement homesCMGODomainRelationshipName 
		detect: CODEElement homeNameCMGODomainAttributeName test: [:aHName | aHName = aHomeName].
	aHomeModel isNil ifTrue: [ ^nil].

	aMetaInfo := aHomeModel metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aRoot := aMetaInfo getObject: aHomeModel featureNamedValue: CODEElement homeRootsCMGODomainAttributeName 
		detect: aRootNameAttributeName  test: [:anModelName | anModelName = aRootName] orCreate: aRootName.
	aRoot  isNil ifTrue: [ ^nil].

	^aRoot! !

!CMAPManager publicMethodsFor: 'ui'!

uiChooseFile: theFileDialogInterface

	| aPilot |
	aPilot := self pilot.
	aPilot isNil ifTrue: [ ^nil].

	^aPilot uiChooseFile: theFileDialogInterface!

uiConfirm: theQuestion

	| aPilot |
	aPilot := self pilot.
	aPilot isNil ifTrue: [ ^false].

	^aPilot uiConfirm: theQuestion!

uiConfirm: theQuestion initialAnswer: theAnswer

	| aPilot |
	aPilot := self pilot.
	aPilot isNil ifTrue: [ ^false].

	^aPilot uiConfirm: theQuestion initialAnswer: theAnswer!

uiCursor: theCursor showWhile: theBlock

	| aPilot |
	theBlock isNil ifTrue: [ ^nil].

	aPilot := self pilot.
	aPilot isNil ifTrue: [ ^theBlock value].

	^aPilot uiCursor: theCursor showWhile: theBlock!

uiMessage: theMessage

	| aPilot |
	aPilot := self pilot.
	aPilot isNil ifTrue: [ ^nil].

	^aPilot uiMessage: theMessage!

uiWarn: theWarning

	| aPilot |
	aPilot := self pilot.
	aPilot isNil ifTrue: [ ^nil].

	^aPilot uiWarn: theWarning!

uiWarnAndMessage: theMessage

	| aPilot |
	aPilot := self pilot.
	aPilot isNil ifTrue: [ ^nil].

	^aPilot uiWarnAndMessage: theMessage! !

!CMAPMETAConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"CMAPMETAConfiguration initialize"

	super initialize! !

!CMAPMETAConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^('Configuracion de Navegadores.\', 
		'Utilize los parametros en esta configuracion para controlar el aspecto de los navegadores de CMAPware') copy withCRs!

configurationName
	^'Navegadores CMAPware' copy! !

!CMAPMETAConfiguration class publicMethodsFor: 'ref:parametersvalues'!

numberOfEditorHoldersParameterValue
	^3!

preferredConfigurationsCollectionClass
	^CMAPConfigurationsCollection! !

!CMAPMetaInfoHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentModel browsePath"
	"self  resetCurrentModels"
	"(self  currentModelStoreMethodSelector: self defaultCurrentModelSelector) browsePath"
	"self  resetCurrentModelStoreMethodSelector: self defaultCurrentModelSelector"!

currentModelStoreMethodSelector: theStoreMethodSelector
	self shouldNotImplement!

currentModelStoreMethodSelector: theStoreMethodSelector withApplicationConfiguration: theApplicationConfiguration
	
	| aModel someCurrentModels |

	aModel := nil.

	someCurrentModels := self currentModels.

	aModel :=  someCurrentModels at:  theStoreMethodSelector ifAbsent: [ nil].

	aModel isNil ifFalse: [ ^aModel].

	aModel := self retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector withApplicationConfiguration: theApplicationConfiguration.
	aModel isNil ifTrue: [ ^nil].

	someCurrentModels at:  theStoreMethodSelector  put: aModel.

	^aModel!

retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector
	self shouldNotImplement!

retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector withApplicationConfiguration: theApplicationConfiguration
	| aModel |
	aModel := super retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector.
	aModel isNil ifTrue: [ ^nil].

	"aModel definitionsHolderClassNameForInstances: EDOCUIDefinitionsHolder name.
	aModel configurationClassNameForInstances: EDOCUIDeveloperConfiguration name."
	^aModel! !

!CMAPMetaInfoHolder class publicMethodsFor: 'default'!

defaultCurrentModelSelector
	"self  defaultCurrentTranslationSelector "

	^#m3SimpleStore! !

!CMAPMetaInfoHolder class publicMethodsFor: 'modelElements persistence'!

cmapStore

	"(CODEElement newFromPersistenceAsCode: CMAPMetaInfoHolder cmapStore) browsePath"

	self ojoModel.

	^   #( model 'CMAP'
	nil nil
	nil
	nil
	CMAPMetaInfoHolder cmapStore
	nil
	(submodules
	  ( module 'DomainRootElements'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Domain'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'domainModelCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEModel' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'domainNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'homesCMGO'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'domainCMGO'  ( refToType 'Home' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Home'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'homeIDCounterCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Integer' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeRootsCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homedElementsTypeMetaInfoCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEType' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'domainCMGO'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'homesCMGO'  ( refToType 'Domain' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'PrimitiveTypes'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'CODEModel'
			nil nil
			nil
			nil
			false false false
			CODEModel nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Integer'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			'1'
			nil
			nil
			nil
			nil
		   )

		  ( type 'CMGO'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'objectDomainCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Domain' 'DomainRootElements'  ) 

			   )

			 )
			nil
			nil
		   )

		  ( type 'CODEType'
			nil nil
			nil
			nil
			false false false
			CODEElement nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'String'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Date'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Time'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Number'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Boolean'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Text'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'SpanishDate'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		 )
		nil
	   )

	  ( module 'Aspectos'
		nil nil
		(definedAspects
		  ( aspect 'PresentationText'
			nil nil
		   )

		 )
		nil
		nil nil
		nil
		nil
	   )

	  ( module 'mda'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'mof'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'ModelElement'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'name'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'NameType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'annotation'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'AnnotationType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'qualifiedName'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false false false false false
					ALWAYS
					''
					''
					''
					'calc container qualifiedNameWithSeparator , "::" , name'
					''
					 ( refToType 'NameType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'qualifiedNameWithSeparator'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					ALWAYS
					''
					''
					''
					'calc qualifiedName , "::"'
					''
					 ( refToType 'NameType' 'mda' 'mof'  ) 

				   )

				 )
				(relationships
				  ( relationship 'container'
					nil nil
					nil
					nil
					isAGGREGATED nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'contents'  ( refToType 'Namespace' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'tag'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'modelElement'  ( refToType 'Tag' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'NameType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false true false false
				nil
				(supertypes
				   ( refToType 'String' 'PrimitiveTypes'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'AnnotationType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false true false false
				nil
				(supertypes
				   ( refToType 'String' 'PrimitiveTypes'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'Namespace'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'contents'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'container'  ( refToType 'ModelElement' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'importer'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'imported'  ( refToType 'Import' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'GeneralizableElement'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Namespace' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'supertype'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'subtype'  ( refToType 'GeneralizableElement' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'subtype'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'supertype'  ( refToType 'GeneralizableElement' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Package'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'GeneralizableElement' 'mda' 'mof'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'Classifier'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'GeneralizableElement' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'typedElement'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'type'  ( refToType 'TypedElement' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Import'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'visibility'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal public'
					''
					 ( refToType 'VisibilityKind' 'mda' 'mof'  ) 

				   )

				 )
				(relationships
				  ( relationship 'imported'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'importer'  ( refToType 'Namespace' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'VisibilityKind'
				nil nil
				nil
				nil
				false true false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'public'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'protected'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'private'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'EvaluationKind'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'immediate'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'deferred'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'Constraint'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'expression'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'language'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'evaluationPolicy'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal immediate'
					''
					 ( refToType 'EvaluationKind' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Tag'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'tagId'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'values'
					nil nil
					nil
					nil
					'' nil
					#'0' #*
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

				   )

				 )
				(relationships
				  ( relationship 'modelElement'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'tag'  ( refToType 'ModelElement' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'TypedElement'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				nil
				nil
				(relationships
				  ( relationship 'type'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'typedElement'  ( refToType 'Classifier' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Association'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'isDerived'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal false'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'TypeDescriptor'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				nil
				nil
				nil
			   )

			  ( type 'DataType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'typeCode'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'TypeDescriptor' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Feature'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'visibility'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal public'
					''
					 ( refToType 'VisibilityKind' 'mda' 'mof'  ) 

				   )

				  ( attribute 'scope'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal instance'
					''
					 ( refToType 'ScopeKind' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'ScopeKind'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'static'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'instance'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'BehavioralFeature'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Namespace' 'mda' 'mof'  ) 
				   ( refToType 'Feature' 'mda' 'mof'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'Operation'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'BehavioralFeature' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'isQuery'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				(relationships
				  ( relationship 'exception'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'operation'  ( refToType 'MofException' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'MofException'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'BehavioralFeature' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'operation'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'exception'  ( refToType 'Operation' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'MofAttribute'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'StructuralFeature' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'is_derived'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal false'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'StructuralFeature'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Feature' 'mda' 'mof'  ) 
				   ( refToType 'TypedElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'multiplicity'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false true true
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'MultiplicityType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'is_changeable'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'MultiplicityType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				(attributes
				  ( attribute 'lower'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal 1'
					''
					 ( refToType 'Integer' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'upper'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal 1'
					''
					 ( refToType 'Integer' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'is_ordered'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'is_unique'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Reference'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'StructuralFeature' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'referencedEnd'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'referent'  ( refToType 'AssociationEnd' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'exposedEnd'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'referer'  ( refToType 'AssociationEnd' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Constant'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'constValue'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'TypeAlias'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'multiplicity'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false true true
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'MultiplicityType' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'AssociationEnd'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'multiplicity'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false true true
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'MultiplicityType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'aggregation'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal none'
					''
					 ( refToType 'AggregationKind' 'mda' 'mof'  ) 

				   )

				  ( attribute 'is_navigable'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'is_changeable'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					nil
				   )

				 )
				(relationships
				  ( relationship 'referent'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'referencedEnd'  ( refToType 'Reference' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'referer'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'exposedEnd'  ( refToType 'Reference' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'AggregationKind'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				(attributes
				  ( attribute 'none'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'shared'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'composite'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'Class'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Classifier' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'is_singleton'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal false'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Model'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false true false
				nil
				(supertypes
				   ( refToType 'Package' 'mda' 'mof'  ) 
				 )
				nil
				nil
				nil
			   )

			 )
			nil
		   )

		 )
	   )

	 )
	CMAPTranslationHolder cmapNoTranslationStore
   )!

m3SimpleStore

	"(CODEElement newFromPersistenceAsCode: CMAPMetaInfoHolder m3SimpleStore) browsePath"

	self ojoModel.

	^   #( model 'M3SimpleModel'
	nil nil
	nil
	nil
	CMAPMetaInfoHolder m3SimpleStore
	(types
	  ( type 'boolean'
		nil nil
		nil
		nil
		false false false
		nil nil
		nil
		false false false false
		nil
		nil
		nil
		nil
		nil
	   )

	 )
	(submodules
	  ( module 'DomainRootElements'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Domain'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'domainModelCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEModel' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'domainNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'homesCMGO'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'domainCMGO'  ( refToType 'Home' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Home'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'homeIDCounterCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Integer' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeRootsCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homedElementsTypeMetaInfoCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEType' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'domainCMGO'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'homesCMGO'  ( refToType 'Domain' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'PrimitiveTypes'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'CODEModel'
			nil nil
			nil
			nil
			false false false
			CODEModel nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Integer'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			'1'
			nil
			nil
			nil
			nil
		   )

		  ( type 'CMGO'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'objectDomainCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Domain' 'DomainRootElements'  ) 

			   )

			 )
			nil
			nil
		   )

		  ( type 'CODEType'
			nil nil
			nil
			nil
			false false false
			CODEElement nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'String'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Date'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Time'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Number'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Boolean'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Text'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'SpanishDate'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		 )
		nil
	   )

	  ( module 'com'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'yourml'
			nil nil
			nil
			nil
			nil nil
			nil
			(submodules
			  ( module 'elec'
				nil nil
				nil
				nil
				nil nil
				nil
				(submodules
				  ( module 'core'
					nil nil
					nil
					nil
					nil nil
					(types
					  ( type 'AbstractElecCoreObject'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
						 )
						nil
						nil
						nil
					   )

					  ( type 'Elecciones'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false true false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'fecha'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'cliente'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'calc cliente , fecha'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'convocatorias'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'consultaElectoral'  ( refToType 'Convocatoria' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'aliasGroups'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'consultaElectoral'  ( refToType 'AliasGroup' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'versionDeltas'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'consultaElectoral'  ( refToType 'VersionDelta' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'elecciones'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Convocatoria'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Convocatoria'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'tipoOrganismo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'TipoOrganismo' 'com' 'yourml' 'elec' 'core' 'enums'  ) 

						   )

						  ( attribute 'orden'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'literal 1'
							''
							 ( refToType 'Number' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc orden , "_" , nombre , "_" , tipoOrganismo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'consultaElectoral'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'convocatorias'  ( refToType 'Elecciones' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'circunscripciones'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'convocatoria'  ( refToType 'Circunscripcion' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidatos'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'convocatoria'  ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'totales'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'convocatoria'  ( refToType 'Totales' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Circunscripcion'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot territorio titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'censo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false true false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'convocatoria'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'circunscripciones'  ( refToType 'Convocatoria' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'mesas'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'circunscripcion'  ( refToType 'Mesa' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidaturas'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'circunscripcion'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'reparto'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'circunscripcion'  ( refToType 'Reparto' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'territorio'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate calc convocatoria consultaElectoral territorios :recurse'
							''
							''
							( refToInverseRelationship 'circunscripcion'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Mesa'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral mesa'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true true false false false false
							ALWAYS
							''
							''
							''
							'clonenot nombre , "_de_" , circunscripcion titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'circunscripcion'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'mesas'  ( refToType 'Circunscripcion' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'recuento'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'mesa'  ( refToType 'Recuento' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'territorio'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'mesas'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Candidatura'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'calc candidato nombreComoCandidatura , "_en_" , circunscripcion titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'candidato'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate circunscripcion convocatoria candidatos'
							''
							''
							( refToInverseRelationship 'candidaturas'  ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidatosEnLista'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false true false false false false
							NOCOMPUTATION
							''
							'nocreate circunscripcion convocatoria candidatos'
							''
							''
							( refToInverseRelationship 'enListaDeCandidaturas'  ( refToType 'Persona' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'circunscripcion'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidaturas'  ( refToType 'Circunscripcion' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidaturasOrdenInferior'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false true false false false false
							NOCOMPUTATION
							''
							'nocreate calc circunscripcion convocatoria consultaElectoral convocatorias circunscripciones candidaturas difference:  circunscripcion candidaturas'
							''
							''
							( refToInverseRelationship 'candidaturaOrdenSuperior'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidaturaOrdenSuperior'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							'nocreate calc circunscripcion convocatoria consultaElectoral convocatorias circunscripciones candidaturas difference:  circunscripcion candidaturas'
							''
							''
							( refToInverseRelationship 'candidaturasOrdenInferior'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'votos'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidatura'  ( refToType 'Votos' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'escanos'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidatura'  ( refToType 'Escanos' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'totalizados'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidatura'  ( refToType 'TotalizadoCandidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Candidato'
						nil nil
						nil
						nil
						true false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'nombreComoCandidatura'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							ALWAYS
							''
							''
							''
							'literal unaCandidatura'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'candidaturas'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidato'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'convocatoria'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidatos'  ( refToType 'Convocatoria' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'aliasGroup'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidatos'  ( refToType 'AliasGroup' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'coaliciones'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'coaligados'  ( refToType 'Coalicion' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'GrupoPolitico'
						nil nil
						nil
						nil
						true false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral GrupoPolitico'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attributeRefinement 'nombreComoCandidatura'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							ALWAYS
							''
							''
							''
							'clonenot nombre'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombreComoCandidatura'  ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
						nil
					   )

					  ( type 'Partido'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'GrupoPolitico' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Partido'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'GrupoPolitico' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
						nil
					   )

					  ( type 'Coalicion'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'GrupoPolitico' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Coalicion'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'GrupoPolitico' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationship 'coaligados'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false true false false false false
							NOCOMPUTATION
							''
							'nocreate convocatoria consultaElectoral convocatorias candidatos'
							''
							''
							( refToInverseRelationship 'coaliciones'  ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Independiente'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Persona' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						nil
						nil
						nil
					   )

					  ( type 'Partidista'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Persona' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						nil
						nil
						nil
					   )

					  ( type 'Persona'
						nil nil
						nil
						nil
						true false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral nombre'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'apellidos'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral apellidos'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'nombreCompleto'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'calc nombre , "_" , apellidos'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attributeRefinement 'nombreComoCandidatura'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							ALWAYS
							''
							''
							''
							'clonenot nombreCompleto'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombreComoCandidatura'  ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationship 'enListaDeCandidaturas'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'candidatosEnLista'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'AliasGroup'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							ALWAYS
							''
							''
							''
							'clonenot candidatos nombreCompleto'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'consultaElectoral'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'aliasGroups'  ( refToType 'Elecciones' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidatos'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false true false false false false
							NOCOMPUTATION
							''
							'nocreate consultaElectoral convocatorias candidatos'
							''
							''
							( refToInverseRelationship 'aliasGroup'  ( refToType 'Candidato' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'VersionDelta'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral version'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'fecha'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'SpanishDate' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'hora'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'Time' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc nombre , "_" , fecha , "_" , hora'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'consultaElectoral'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'versionDeltas'  ( refToType 'Elecciones' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'recuentos'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'versionDelta'  ( refToType 'Recuento' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'repartos'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'versionDelta'  ( refToType 'Reparto' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'totales'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'versionDelta'  ( refToType 'Totales' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Recuento'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc mesa nombre , "_" , version titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'versionDelta'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'recuentos'  ( refToType 'VersionDelta' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'votos'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'recuento'  ( refToType 'Votos' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'mesa'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate version consultaElectoral convocatorias circunscripciones mesas'
							''
							''
							( refToInverseRelationship 'recuento'  ( refToType 'Mesa' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Votos'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc candidatura , "_" , recuento titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'numeroVotos'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false true false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'recuento'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'votos'  ( refToType 'Recuento' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidatura'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate recuento mesa circunscripcion candidaturas'
							''
							''
							( refToInverseRelationship 'votos'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Reparto'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc circunscripcion nombre , "_" , version titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'circunscripcion'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'clonenot version consultaElectoral convocatorias circunscripciones'
							''
							''
							( refToInverseRelationship 'reparto'  ( refToType 'Circunscripcion' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'escanos'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'reparto'  ( refToType 'Escanos' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'versionDelta'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'repartos'  ( refToType 'VersionDelta' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Escanos'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc candidatura  nombre , "_" , reparto titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'numeroEscanos'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false true false true true
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							''
							''
							 ( refToType 'Integer' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'reparto'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'escanos'  ( refToType 'Reparto' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'candidatura'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate reparto circunscripcion candidaturas'
							''
							''
							( refToInverseRelationship 'escanos'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Territorio'
						nil nil
						nil
						nil
						true false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Territorio'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc nombre , ".." , territorio titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'elecciones'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorios'  ( refToType 'Elecciones' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'territorio'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'totales'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'Totales' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'mesas'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							'nocreate elecciones convocatorias circunscripciones mesas'
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'Mesa' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'circunscripcion'
							nil nil
							nil
							nil
							REFERENCES nil
							#'0' #*
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'Circunscripcion' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Totales'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'titulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc versionDelta titulo , "_en_" , territorio nombre'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'versionDelta'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'totales'  ( refToType 'VersionDelta' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'territorio'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate calc versionDelta consultaElectoral territorios :recursecollect'
							''
							''
							( refToInverseRelationship 'totales'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'convocatoria'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate versionDelta consultaElectoral convocatorias'
							''
							''
							( refToInverseRelationship 'totales'  ( refToType 'Convocatoria' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						  ( relationship 'totalizados'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'totales'  ( refToType 'Totalizado' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Totalizado'
						nil nil
						nil
						nil
						true false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'AbstractElecCoreObject' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attribute 'tiulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							ALWAYS
							''
							''
							''
							'clonenot totales titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'censo'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false true true
							NOCOMPUTATION
							''
							''
							''
							''
							''
							 ( refToType 'Number' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'numeroVotos'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false true true
							NOCOMPUTATION
							''
							''
							''
							''
							''
							 ( refToType 'Number' 'PrimitiveTypes'  ) 

						   )

						  ( attribute 'numeroEscanos'
							nil nil
							nil
							nil
							'' nil
							#'0' #'1'
							false false false false true false true true
							NOCOMPUTATION
							''
							''
							''
							''
							''
							 ( refToType 'Number' 'PrimitiveTypes'  ) 

						   )

						 )
						(relationships
						  ( relationship 'totales'
							nil nil
							nil
							nil
							isAGGREGATED nil
							#'1' #'1'
							false false false false false false false
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'totalizados'  ( refToType 'Totales' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'TotalizadoCandidatura'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Totalizado' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attributeRefinement 'tiulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc totales titulo , "_para_" , candidatura nombre'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'tiulo'  ( refToType 'Totalizado' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationship 'candidatura'
							nil nil
							nil
							nil
							REFERENCES nil
							#'1' #'1'
							false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							'nocreate totales versionDelta consultaElectoral convocatorias circunscripciones candidaturas'
							''
							''
							( refToInverseRelationship 'totalizados'  ( refToType 'Candidatura' 'com' 'yourml' 'elec' 'core'  )  ) 
						   )

						 )
						nil
					   )

					  ( type 'Colegio'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						   ( refToType 'DistritoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Colegio'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false true false false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'ColegioContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					  ( type 'Distrito'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						   ( refToType 'MunicipioContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'ComarcaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'ProvinciaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'AutonomiaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'EstadoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Distrito'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'DistritoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					  ( type 'Municipio'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						   ( refToType 'ComarcaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'ProvinciaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'AutonomiaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'EstadoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Municipio'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'MunicipioContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					  ( type 'Comarca'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						   ( refToType 'ProvinciaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'AutonomiaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'EstadoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Comarca'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'ComarcaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					  ( type 'Provincia'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						   ( refToType 'AutonomiaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						   ( refToType 'EstadoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Provincia'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'ProvinciaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					  ( type 'Autonomia'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						   ( refToType 'EstadoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Autonomia'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'AutonomiaContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					  ( type 'Estado'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attributeRefinement 'nombre'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Estado'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'nombre'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false false true false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'EstadoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					  ( type 'TotalizadoTagged'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Totalizado' 'com' 'yourml' 'elec' 'core'  ) 
						 )
						(attributes
						  ( attributeRefinement 'tiulo'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false true false false false false false
							ALWAYS
							''
							''
							''
							'clonenot calc tagName , "_" , totales titulo'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

							(refinedAttributes
							   ( refToAttribute 'tiulo'  ( refToType 'Totalizado' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						  ( attribute 'tagName'
							nil nil
							nil
							nil
							'' nil
							#'1' #'1'
							false false false false false false false false
							INITIALIZEDINCONSTRUCTOR
							''
							''
							''
							'ranliteral Tag'
							''
							 ( refToType 'String' 'PrimitiveTypes'  ) 

						   )

						 )
						nil
						nil
					   )

					  ( type 'Seccion'
						nil nil
						nil
						nil
						false false false
						nil nil
						nil
						false false false false
						nil
						(supertypes
						   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
						   ( refToType 'DistritoContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  ) 
						 )
						nil
						(relationships
						  ( relationshipRefinement 'territorios'
							nil nil
							nil
							nil
							AGGREGATES nil
							#'0' #*
							false true false false false true true
							NOCOMPUTATION
							''
							''
							''
							''
							( refToInverseRelationship 'territorio'  ( refToType 'SeccionContents' 'com' 'yourml' 'elec' 'core' 'territoriosconstraints'  )  ) 
							(refinedRelationships
							  ( refToRefinedRelationship 'territorios'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
							 )
						   )

						 )
						nil
					   )

					 )
					(submodules
					  ( module 'enums'
						nil nil
						nil
						nil
						nil nil
						(types
						  ( type 'TipoOrganismo'
							nil nil
							nil
							nil
							false true false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
							 )
							(attributes
							  ( attribute 'Parlamento Estado'
								nil nil
								nil
								nil
								'' nil
								#'0' #'1'
								false false false false true false true true
								NOCOMPUTATION
								''
								''
								''
								''
								''
								nil
							   )

							  ( attribute 'Parlamento Autonomico'
								nil nil
								nil
								nil
								'' nil
								#'0' #'1'
								false false false false true false true true
								NOCOMPUTATION
								''
								''
								''
								''
								''
								nil
							   )

							  ( attribute 'Municipales'
								nil nil
								nil
								nil
								'' nil
								#'0' #'1'
								false false false false true false true true
								NOCOMPUTATION
								''
								''
								''
								''
								''
								nil
							   )

							  ( attribute 'Consejo Comarcal'
								nil nil
								nil
								nil
								'' nil
								#'0' #'1'
								false false false false true false true true
								NOCOMPUTATION
								''
								''
								''
								''
								''
								nil
							   )

							  ( attribute 'Diputacion Provincial'
								nil nil
								nil
								nil
								'' nil
								#'0' #'1'
								false false false false true false true true
								NOCOMPUTATION
								''
								''
								''
								''
								''
								nil
							   )

							 )
							nil
							nil
						   )

						 )
						nil
					   )

					  ( module 'territoriosconstraints'
						nil nil
						nil
						nil
						nil nil
						(types
						  ( type 'EstadoContents'
							nil nil
							nil
							nil
							true false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Estado' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						  ( type 'AutonomiaContents'
							nil nil
							nil
							nil
							true false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Autonomia' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						  ( type 'ProvinciaContents'
							nil nil
							nil
							nil
							true false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Provincia' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						  ( type 'ComarcaContents'
							nil nil
							nil
							nil
							true false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Comarca' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						  ( type 'MunicipioContents'
							nil nil
							nil
							nil
							true false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Municipio' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						  ( type 'DistritoContents'
							nil nil
							nil
							nil
							true false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Distrito' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						  ( type 'ColegioContents'
							nil nil
							nil
							nil
							true false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Colegio' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						  ( type 'SeccionContents'
							nil nil
							nil
							nil
							false false false
							nil nil
							nil
							false false false false
							nil
							(supertypes
							   ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  ) 
							 )
							nil
							(relationships
							  ( relationshipRefinement 'territorio'
								nil nil
								nil
								nil
								isAGGREGATED nil
								#'1' #'1'
								false false false false false false false
								NOCOMPUTATION
								''
								''
								''
								''
								( refToInverseRelationship 'territorios'  ( refToType 'Seccion' 'com' 'yourml' 'elec' 'core'  )  ) 
								(refinedRelationships
								  ( refToRefinedRelationship 'territorio'  ( refToType 'Territorio' 'com' 'yourml' 'elec' 'core'  )  ) 
								 )
							   )

							 )
							nil
						   )

						 )
						nil
					   )

					 )
				   )

				 )
			   )

			 )
		   )

		 )
	   )

	 )
	CMAPTranslationHolder m3SimpleNoTranslationStore
   )!

omgedocSimpleStore

	"(CODEElement newFromPersistenceAsCode: CMAPMetaInfoHolder omgedocSimpleStore) browsePath"

	self ojoModel.

	^   #( model 'CMAP'
	nil nil
	nil
	nil
	CMAPMetaInfoHolder omgedocSimpleStore
	nil
	(submodules
	  ( module 'DomainRootElements'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Domain'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'domainModelCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEModel' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'domainNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'homesCMGO'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'domainCMGO'  ( refToType 'Home' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Home'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'homeIDCounterCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Integer' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeRootsCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homedElementsTypeMetaInfoCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEType' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'domainCMGO'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'homesCMGO'  ( refToType 'Domain' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'PrimitiveTypes'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'CODEModel'
			nil nil
			nil
			nil
			false false false
			CODEModel nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Integer'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			'1'
			nil
			nil
			nil
			nil
		   )

		  ( type 'CMGO'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'objectDomainCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Domain' 'DomainRootElements'  ) 

			   )

			 )
			nil
			nil
		   )

		  ( type 'CODEType'
			nil nil
			nil
			nil
			false false false
			CODEElement nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'String'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Date'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Time'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Number'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Boolean'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Text'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'SpanishDate'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		 )
		nil
	   )

	  ( module 'Aspectos'
		nil nil
		(definedAspects
		  ( aspect 'PresentationText'
			nil nil
		   )

		 )
		nil
		nil nil
		nil
		nil
	   )

	  ( module 'mda'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'eca'
			nil nil
			nil
			nil
			nil nil
			nil
			(submodules
			  ( module 'cca'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'ProcessComponent'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Composition' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral ProcessComponent'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'granularity'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal program'
						''
						nil
					   )

					  ( attribute 'isPersistent'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'primitiveKind'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'primitiveSpec'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal PC'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'allProperties'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc properties union: supertype properties'
						''
						 ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'properties'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'component'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'usedByComponentUsages'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'uses'  ( refToType 'ComponentUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'bindsToOfContextualBindings'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'bindsTo'  ( refToType 'ContextualBinding' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'Package'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PackageContent' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Package'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PKG'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'ownedElements'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'namespace'  ( refToType 'PackageContent' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Composition'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Choreography' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationship 'uses'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'ComponentUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'bindings'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'ContextualBinding' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'UsageContext'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					nil
					(relationships
					  ( relationship 'portsUsed'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'extent'  ( refToType 'PortUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'GranularityKind'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'program'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'owned'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'shared'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'PackageContent'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					(relationships
					  ( relationship 'namespace'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ownedElements'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'DataElement'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'PackageContent' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationship 'typeOfFlowPorts'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'type'  ( refToType 'FlowPort' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'typeOfPropertyDefinitions'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'type'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'constraints'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'constrainedElement'  ( refToType 'DataInvariant' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'typeOfAttributes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'type'  ( refToType 'Attribute' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'ElementImport'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PackageContent' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					nil
					nil
				   )

				  ( type 'CommunityProcess'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Composition' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal COMM'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral CommunityProcess'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'CommunityProcess' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'CommunityProcess' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'Protocol'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Choreography' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Protocol'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PROT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'initiator'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #'1'
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'protocol'  ( refToType 'InitiatingRole' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'responder'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #'1'
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'protocol'  ( refToType 'RespondingRole' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'usedByProtocolPorts'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'uses'  ( refToType 'ProtocolPort' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'Model'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false true false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Model'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'Choreography'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					(relationships
					  ( relationship 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'nodes'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'choreography'  ( refToType 'Node' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'abstractTransitions'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'choreography'  ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PortOwner'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot ports'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Port'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'isSynchronous'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'isTransactional'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal initiates'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attribute 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal anyStatus'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'usedByPortUsages'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'represents'  ( refToType 'PortUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'MultiPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral MultiPort'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal MUPO'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'MultiPortFlowPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'OperationPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral OperationPort'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'ProtocolPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal a'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name , ":" , uses name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PRPO'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'uses'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'usedByProtocolPorts'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'FlowPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal a'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name , ":" , type name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal FLOW'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'type'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'typeOfFlowPorts'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'typeProperty'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						''
						'nocreate owner properties'
						''
						( refToInverseRelationship 'constrains'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'InitiatingRole'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal Initiator'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal INIT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'protocol'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'initiator'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'RespondingRole'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal Responder'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal RESP'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'protocol'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'responder'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Interface'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Protocol' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Interface'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'InterfaceOperationPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'Interface' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'Interface' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'PropertyDefinition'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'initial'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						 ( refToType 'Expression' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attribute 'isLocked'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						INITIALVALUE
						''
						''
						''
						'clonenot false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral PropertyDefinition'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'component'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'properties'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'type'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'typeOfPropertyDefinitions'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'constrains'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'typeProperty'  ( refToType 'FlowPort' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'filledByPropertyValues'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'fills'  ( refToType 'PropertyValue' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'DirectionType'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'initiates'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'responds'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'Expression'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false true false false
					nil
					(supertypes
					   ( refToType 'String' 'PrimitiveTypes'  ) 
					 )
					nil
					nil
					nil
				   )

				  ( type 'NamedElement'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					   ( refToType 'Labeled' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					 )
					nil
					nil
				   )

				  ( type 'Status'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'success'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'timeoutFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'technicalFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'businessFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'anyFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'anyStatus'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationCallFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal initiates'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal anyStatus'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'postCondition'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationReturnFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal responds'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal success'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'postCondition'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationExceptionFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal responds'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal anyFailure'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'postCondition'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints 
As a PortOwner, the OperationPort:
 May only contain FlowPorts.
 Must contain exactly one flow port with direction set to "responds."
 Must contain exactly one flow port with direction set to initiates (the call).'
					   )

					 )

					nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'FlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'OperationPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'MultiPortFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'FlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'MultiPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'InterfaceOperationPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraint 
The Ports related by the Ports association must;
be of type OperationPort or FlowPort.
have direction == responds.'
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal responds'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'Interface' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'DataInvariant'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'expression'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'onCommit'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						nil
					   )

					 )
					(relationships
					  ( relationship 'constrainedElement'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'constraints'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Enumeration'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Enumeration'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'values'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'enumeration'  ( refToType 'EnumerationValue' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'initial'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'clonenot values'
						''
						''
						( refToInverseRelationship 'initialOfEnumeration'  ( refToType 'EnumerationValue' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'EnumerationValue'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral EnumerationValue'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'enumeration'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'values'  ( refToType 'Enumeration' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'initialOfEnumeration'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'initial'  ( refToType 'Enumeration' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'DataType'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral DataType'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal DATY'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'ExternalDocument'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataType' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral ExternalDocument'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'DataType' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'mimeType'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'specURL'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'externalName'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attributeRefinement 'kind'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal DOC'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'DataType' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'CompositeData'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral CompositeData'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal CDAT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'allFeatures'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc features union: supertype allFeatures'
						''
						 ( refToType 'Attribute' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'CompositeData' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'CompositeData' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'features'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'Attribute' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Attribute'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'byValue'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'required'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'many'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'initialValue'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Expression' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Attribute'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc kind , "-" , name  , ":" , type name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'features'  ( refToType 'CompositeData' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'type'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'typeOfAttributes'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Node'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationship 'choreography'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'nodes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'outgoing'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'source'  ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'incoming'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'target'  ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'AbstractTransition'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					   ( refToType 'Labeled' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , source displayName , ".>." , target displayName'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					 )
					(relationships
					  ( relationship 'choreography'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'abstractTransitions'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'source'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate choreography nodes'
						''
						''
						( refToInverseRelationship 'outgoing'  ( refToType 'Node' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'target'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate choreography nodes'
						''
						''
						( refToInverseRelationship 'incoming'  ( refToType 'Node' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PseudoState'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Node' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'kind'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal initial'
						''
						 ( refToType 'PseudostateKind' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false true false true
						ALWAYS
						''
						''
						''
						'calc "PS_" , kind'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'PortUsage'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Node' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false true
						ALWAYS
						''
						''
						''
						'calc label , "-" , name  , ":" , represents displayName'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'extent'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate 2 calc choreography union: choreography uses union: choreography nodes '
						''
						''
						( refToInverseRelationship 'portsUsed'  ( refToType 'UsageContext' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'represents'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate 2 calc extent ports union: extent uses ports union: extent represents ports union: extent represents uses ports'
						''
						''
						( refToInverseRelationship 'usedByPortUsages'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Connection'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal CONN'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'Transition'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'preCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal TRAN'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'PseudostateKind'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'choice'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'fork'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'initial'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'join'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'success'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'failure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'PortActivity'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortUsage' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral PortActivity'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PACT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'ComponentUsage'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral ComponentUsage'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name , ":" , uses name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal CUSE'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'uses'  ( refToType 'Composition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'uses'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'usedByComponentUsages'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'fillsContextualBindings'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'fills'  ( refToType 'ContextualBinding' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PortConnector'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortUsage' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral PortConnector'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PCON'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'ContextualBinding'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'bindings'  ( refToType 'Composition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'fills'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'fillsContextualBindings'  ( refToType 'ComponentUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'bindsTo'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'bindsToOfContextualBindings'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PropertyValue'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'value'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Expression' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'fills'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'filledByPropertyValues'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Labeled'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal ?'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					 )
					nil
					nil
				   )

				 )
				nil
			   )

			  ( module 'entities'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			  ( module 'events'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			  ( module 'process'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			  ( module 'relationships'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			 )
		   )

		 )
	   )

	 )
	CMAPTranslationHolder omgedocSimpleNoTranslationStore
   )!

omgedocSimpleStoreWithoutMOF

	"(CODEElement newFromPersistenceAsCode: CMAPMetaInfoHolder omgedocSimpleStoreWithoutMOF) browsePath"

	self ojoModel.

	^   #( model 'CMAP'
	nil nil
	nil
	nil
	CMAPMetaInfoHolder omgedocSimpleStore
	nil
	(submodules
	  ( module 'DomainRootElements'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Domain'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'domainModelCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEModel' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'domainNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'homesCMGO'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'domainCMGO'  ( refToType 'Home' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Home'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'homeIDCounterCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Integer' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeRootsCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homedElementsTypeMetaInfoCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEType' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'domainCMGO'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'homesCMGO'  ( refToType 'Domain' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'PrimitiveTypes'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'CODEModel'
			nil nil
			nil
			nil
			false false false
			CODEModel nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Integer'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			'1'
			nil
			nil
			nil
			nil
		   )

		  ( type 'CMGO'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'objectDomainCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Domain' 'DomainRootElements'  ) 

			   )

			 )
			nil
			nil
		   )

		  ( type 'CODEType'
			nil nil
			nil
			nil
			false false false
			CODEElement nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'String'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Date'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Time'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Number'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Boolean'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Text'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'SpanishDate'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		 )
		nil
	   )

	  ( module 'Aspectos'
		nil nil
		(definedAspects
		  ( aspect 'PresentationText'
			nil nil
		   )

		 )
		nil
		nil nil
		nil
		nil
	   )

	  ( module 'mda'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'eca'
			nil nil
			nil
			nil
			nil nil
			nil
			(submodules
			  ( module 'cca'
				nil nil
				nil
				nil
				nil nil
				(types
				  ( type 'ProcessComponent'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Composition' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral ProcessComponent'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'granularity'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal program'
						''
						nil
					   )

					  ( attribute 'isPersistent'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'primitiveKind'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'primitiveSpec'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal PC'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'allProperties'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc properties union: supertype properties'
						''
						 ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'properties'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'component'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'usedByComponentUsages'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'uses'  ( refToType 'ComponentUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'bindsToOfContextualBindings'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'bindsTo'  ( refToType 'ContextualBinding' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'Package'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PackageContent' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Package'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PKG'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'ownedElements'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'namespace'  ( refToType 'PackageContent' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Composition'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Choreography' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationship 'uses'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'ComponentUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'bindings'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'ContextualBinding' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'UsageContext'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					nil
					nil
					(relationships
					  ( relationship 'portsUsed'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'extent'  ( refToType 'PortUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'GranularityKind'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'program'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'owned'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'shared'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'PackageContent'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					(relationships
					  ( relationship 'namespace'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ownedElements'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'DataElement'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'PackageContent' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationship 'typeOfFlowPorts'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'type'  ( refToType 'FlowPort' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'typeOfPropertyDefinitions'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'type'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'constraints'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'constrainedElement'  ( refToType 'DataInvariant' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'typeOfAttributes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'type'  ( refToType 'Attribute' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'ElementImport'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PackageContent' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					nil
					nil
				   )

				  ( type 'CommunityProcess'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Composition' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal COMM'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral CommunityProcess'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'CommunityProcess' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'CommunityProcess' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'Protocol'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Choreography' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Protocol'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PROT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'initiator'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #'1'
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'protocol'  ( refToType 'InitiatingRole' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'responder'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #'1'
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'protocol'  ( refToType 'RespondingRole' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'usedByProtocolPorts'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'uses'  ( refToType 'ProtocolPort' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'Model'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false true false
					nil
					(supertypes
					   ( refToType 'Package' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Model'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Package' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'Choreography'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					(relationships
					  ( relationship 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'nodes'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'choreography'  ( refToType 'Node' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'abstractTransitions'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'choreography'  ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PortOwner'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'clonenot ports'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Port'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'isSynchronous'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'isTransactional'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal initiates'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attribute 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal anyStatus'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'usedByPortUsages'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'represents'  ( refToType 'PortUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'MultiPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral MultiPort'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal MUPO'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'MultiPortFlowPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'OperationPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortOwner' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral OperationPort'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'allPorts'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'calc ports union: supertype allPorts'
						''
						 ( refToType 'Port' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'allPorts'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'ProtocolPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal a'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name , ":" , uses name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PRPO'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'uses'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'usedByProtocolPorts'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'FlowPort'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Port' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal a'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name , ":" , type name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal FLOW'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'type'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'typeOfFlowPorts'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'typeProperty'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						''
						'nocreate owner properties'
						''
						( refToInverseRelationship 'constrains'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'InitiatingRole'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal Initiator'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal INIT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'protocol'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'initiator'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'RespondingRole'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal Responder'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal RESP'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'protocol'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'responder'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Interface'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Protocol' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Interface'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'ports'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'InterfaceOperationPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'ports'  ( refToType 'PortOwner' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'Interface' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'supertype'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( relationshipRefinement 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'Interface' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'subtypes'  ( refToType 'Protocol' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'PropertyDefinition'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'initial'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						 ( refToType 'Expression' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attribute 'isLocked'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						INITIALVALUE
						''
						''
						''
						'clonenot false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral PropertyDefinition'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'component'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'properties'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'type'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'typeOfPropertyDefinitions'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'constrains'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'typeProperty'  ( refToType 'FlowPort' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'filledByPropertyValues'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'fills'  ( refToType 'PropertyValue' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'DirectionType'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'initiates'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'responds'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'Expression'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false true false false
					nil
					(supertypes
					   ( refToType 'String' 'PrimitiveTypes'  ) 
					 )
					nil
					nil
					nil
				   )

				  ( type 'NamedElement'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					   ( refToType 'Labeled' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					 )
					nil
					nil
				   )

				  ( type 'Status'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'success'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'timeoutFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'technicalFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'businessFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'anyFailure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'anyStatus'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationCallFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal initiates'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal anyStatus'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'postCondition'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationReturnFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal responds'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal success'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'postCondition'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationExceptionFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationFlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal responds'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'postCondition'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal anyFailure'
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'postCondition'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'OperationFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints 
As a PortOwner, the OperationPort:
 May only contain FlowPorts.
 Must contain exactly one flow port with direction set to "responds."
 Must contain exactly one flow port with direction set to initiates (the call).'
					   )

					 )

					nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'FlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'OperationPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'MultiPortFlowPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraints '
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'FlowPort' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationshipRefinement 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'MultiPort' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'InterfaceOperationPort'
					(comments

					  ( comment 'Description'
						'Created to partially enforce constraint 
The Ports related by the Ports association must;
be of type OperationPort or FlowPort.
have direction == responds.'
					   )

					 )

					nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'OperationPort' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'direction'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						INITIALVALUE
						''
						''
						''
						'literal responds'
						''
						 ( refToType 'DirectionType' 'mda' 'eca' 'cca'  ) 

						(refinedAttributes
						   ( refToAttribute 'direction'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationshipRefinement 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'ports'  ( refToType 'Interface' 'mda' 'eca' 'cca'  )  ) 
						(refinedRelationships
						  ( refToRefinedRelationship 'owner'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
				   )

				  ( type 'DataInvariant'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'expression'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'onCommit'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						nil
					   )

					 )
					(relationships
					  ( relationship 'constrainedElement'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'constraints'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Enumeration'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Enumeration'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'values'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'enumeration'  ( refToType 'EnumerationValue' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'initial'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'clonenot values'
						''
						''
						( refToInverseRelationship 'initialOfEnumeration'  ( refToType 'EnumerationValue' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'EnumerationValue'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral EnumerationValue'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'enumeration'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'values'  ( refToType 'Enumeration' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'initialOfEnumeration'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'initial'  ( refToType 'Enumeration' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'DataType'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral DataType'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal DATY'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'ExternalDocument'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataType' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral ExternalDocument'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'DataType' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'mimeType'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'specURL'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'externalName'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					  ( attributeRefinement 'kind'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal DOC'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'DataType' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'CompositeData'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'DataElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral CompositeData'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal CDAT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attribute 'allFeatures'
						nil nil
						nil
						nil
						'' nil
						#'0' #*
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'calc features union: supertype allFeatures'
						''
						 ( refToType 'Attribute' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'subtypes'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'supertype'  ( refToType 'CompositeData' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'supertype'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						NOCOMPUTATION
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'subtypes'  ( refToType 'CompositeData' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'features'
						nil nil
						nil
						nil
						AGGREGATES nil
						#'0' #*
						false false true false false true true
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'owner'  ( refToType 'Attribute' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Attribute'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'byValue'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal true'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'required'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'many'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal false'
						''
						 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

					   )

					  ( attribute 'initialValue'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Expression' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral Attribute'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc kind , "-" , name  , ":" , type name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'features'  ( refToType 'CompositeData' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'type'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'typeOfAttributes'  ( refToType 'DataElement' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Node'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					 )
					nil
					(relationships
					  ( relationship 'choreography'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'nodes'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'outgoing'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'source'  ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'incoming'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'target'  ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'AbstractTransition'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					   ( refToType 'Labeled' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , source displayName , ".>." , target displayName'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					 )
					(relationships
					  ( relationship 'choreography'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'abstractTransitions'  ( refToType 'Choreography' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'source'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate choreography nodes'
						''
						''
						( refToInverseRelationship 'outgoing'  ( refToType 'Node' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'target'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate choreography nodes'
						''
						''
						( refToInverseRelationship 'incoming'  ( refToType 'Node' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PseudoState'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Node' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'kind'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'literal initial'
						''
						 ( refToType 'PseudostateKind' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false true false true
						ALWAYS
						''
						''
						''
						'calc "PS_" , kind'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'PortUsage'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'Node' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false true
						ALWAYS
						''
						''
						''
						'calc label , "-" , name  , ":" , represents displayName'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'extent'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate 2 calc choreography union: choreography uses union: choreography nodes '
						''
						''
						( refToInverseRelationship 'portsUsed'  ( refToType 'UsageContext' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'represents'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate 2 calc extent ports union: extent uses ports union: extent represents ports union: extent represents uses ports'
						''
						''
						( refToInverseRelationship 'usedByPortUsages'  ( refToType 'Port' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Connection'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal CONN'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'Transition'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'AbstractTransition' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attribute 'preCondition'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						 ( refToType 'Status' 'mda' 'eca' 'cca'  ) 

					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false true true
						ALWAYS
						''
						''
						''
						'literal TRAN'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'PseudostateKind'
					nil nil
					nil
					nil
					false true false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'choice'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'fork'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'initial'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'join'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'success'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					  ( attribute 'failure'
						nil nil
						nil
						nil
						'' nil
						#'0' #'1'
						false false false false true false true true
						NOCOMPUTATION
						''
						''
						''
						''
						''
						nil
					   )

					 )
					nil
					nil
				   )

				  ( type 'PortActivity'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortUsage' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral PortActivity'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PACT'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'ComponentUsage'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'NamedElement' 'mda' 'eca' 'cca'  ) 
					   ( refToType 'UsageContext' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false true true
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral ComponentUsage'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'displayName'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false true false false false false false
						ALWAYS
						''
						''
						''
						'calc label , "-" , name , ":" , uses name'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'displayName'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal CUSE'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'uses'  ( refToType 'Composition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'uses'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false false false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						'nocreate $extent'
						''
						''
						( refToInverseRelationship 'usedByComponentUsages'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'fillsContextualBindings'
						nil nil
						nil
						nil
						REFERENCES nil
						#'0' #*
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'fills'  ( refToType 'ContextualBinding' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PortConnector'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'PortUsage' 'mda' 'eca' 'cca'  ) 
					 )
					(attributes
					  ( attributeRefinement 'name'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						'ranliteral PortConnector'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'name'  ( refToType 'NamedElement' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					  ( attributeRefinement 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal PCON'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

						(refinedAttributes
						   ( refToAttribute 'label'  ( refToType 'Labeled' 'mda' 'eca' 'cca'  )  ) 
						 )
					   )

					 )
					nil
					nil
				   )

				  ( type 'ContextualBinding'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					nil
					(relationships
					  ( relationship 'owner'
						nil nil
						nil
						nil
						isAGGREGATED nil
						#'1' #'1'
						false false false false false false false
						NOCOMPUTATION
						''
						''
						''
						''
						( refToInverseRelationship 'bindings'  ( refToType 'Composition' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'fills'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'fillsContextualBindings'  ( refToType 'ComponentUsage' 'mda' 'eca' 'cca'  )  ) 
					   )

					  ( relationship 'bindsTo'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'bindsToOfContextualBindings'  ( refToType 'ProcessComponent' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'PropertyValue'
					nil nil
					nil
					nil
					false false false
					nil nil
					nil
					false false false false
					nil
					(supertypes
					   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
					 )
					(attributes
					  ( attribute 'value'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false true false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						''
						 ( refToType 'Expression' 'mda' 'eca' 'cca'  ) 

					   )

					 )
					(relationships
					  ( relationship 'fills'
						nil nil
						nil
						nil
						REFERENCES nil
						#'1' #'1'
						false false true false false false false
						INITIALIZEDINCONSTRUCTOR
						''
						''
						''
						''
						( refToInverseRelationship 'filledByPropertyValues'  ( refToType 'PropertyDefinition' 'mda' 'eca' 'cca'  )  ) 
					   )

					 )
					nil
				   )

				  ( type 'Labeled'
					nil nil
					nil
					nil
					true false false
					nil nil
					nil
					false false false false
					nil
					nil
					(attributes
					  ( attribute 'label'
						nil nil
						nil
						nil
						'' nil
						#'1' #'1'
						false false false false false false false false
						ALWAYS
						''
						''
						''
						'literal ?'
						''
						 ( refToType 'String' 'PrimitiveTypes'  ) 

					   )

					 )
					nil
					nil
				   )

				 )
				nil
			   )

			  ( module 'entities'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			  ( module 'events'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			  ( module 'process'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			  ( module 'relationships'
				nil nil
				nil
				nil
				nil nil
				nil
				nil
			   )

			 )
		   )

		 )
	   )

	 )
	CMAPTranslationHolder omgedocSimpleNoTranslationStore
   )!

repostoryGroveSimpleStore

	"(CODEElement newFromPersistenceAsCode: CMAPMetaInfoHolder repostoryGroveSimpleStore) browsePath"

	self ojoModel.

	^   #( model 'GroveModel'
	nil nil
	nil
	nil
	CMAPMetaInfoHolder repostoryGroveSimpleStore
	(types
	  ( type 'Boolean'
		nil nil
		nil
		nil
		true false false
		nil nil
		nil
		false false false false
		nil
		nil
		nil
		nil
		nil
	   )

	  ( type 'Object'
		nil nil
		nil
		nil
		false false false
		nil nil
		nil
		false false false false
		nil
		nil
		nil
		nil
		nil
	   )

	 )
	(submodules
	  ( module 'core'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Green'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'tags'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'green'  ( refToType 'Tag' 'core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Stem'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Green' 'core'  ) 
			 )
			nil
			(relationships
			  ( relationship 'sprouts'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'stem'  ( refToType 'Sprout' 'core'  )  ) 
			   )

			  ( relationship 'references'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'stems'  ( refToType 'Reference' 'core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Sprout'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Green' 'core'  ) 
			 )
			nil
			(relationships
			  ( relationship 'stem'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'sprouts'  ( refToType 'Stem' 'core'  )  ) 
			   )

			  ( relationship 'complex'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'sprouts'  ( refToType 'Complex' 'core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Terminal'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Sprout' 'core'  ) 
			 )
			(attributes
			  ( attribute 'value'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Object'  ) 

			   )

			 )
			nil
			nil
		   )

		  ( type 'Multiple'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Sprout' 'core'  ) 
			 )
			nil
			(relationships
			  ( relationship 'others'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'others'  ( refToType 'Multiple' 'core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Complex'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Sprout' 'core'  ) 
			 )
			nil
			(relationships
			  ( relationship 'sprouts'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'complex'  ( refToType 'Sprout' 'core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Reference'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Sprout' 'core'  ) 
			 )
			nil
			(relationships
			  ( relationship 'stems'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'references'  ( refToType 'Stem' 'core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Directed'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Multiple' 'core'  ) 
			 )
			(attributes
			  ( attribute 'isPrev'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Boolean'  ) 

			   )

			 )
			nil
			nil
		   )

		  ( type 'Tag'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'value'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Object'  ) 

			   )

			 )
			(relationships
			  ( relationship 'green'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'tags'  ( refToType 'Green' 'core'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'derived'
		nil nil
		nil
		nil
		nil nil
		nil
		nil
	   )

	 )
	CMAPTranslationHolder m3SimpleNoTranslationStore
   )!

sampleBusinessMetamodel01

	"(CODEElement newFromPersistenceAsCode: CMAPMetaInfoHolder sampleBusinessMetamodel01) browsePath"

	self ojoModel.

	^   #( model 'CMAP'
	nil nil
	nil
	nil
	CMAPMetaInfoHolder sampleBusinessMetamodel01
	nil
	(submodules
	  ( module 'DomainRootElements'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Domain'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'domainModelCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEModel' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'domainNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'homesCMGO'
				nil nil
				nil
				nil
				AGGREGATES nil
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'domainCMGO'  ( refToType 'Home' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Home'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'homeIDCounterCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Integer' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeRootsCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #*
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homedElementsTypeMetaInfoCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'CODEType' 'PrimitiveTypes'  ) 

			   )

			  ( attribute 'homeNameCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false true false true false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'String' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'domainCMGO'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'homesCMGO'  ( refToType 'Domain' 'DomainRootElements'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'PrimitiveTypes'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'CODEModel'
			nil nil
			nil
			nil
			false false false
			CODEModel nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Integer'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			'1'
			nil
			nil
			nil
			nil
		   )

		  ( type 'CMGO'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			nil
			(attributes
			  ( attribute 'objectDomainCMGO'
				nil nil
				nil
				nil
				'' nil
				#'0' #'1'
				false false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				''
				 ( refToType 'Domain' 'DomainRootElements'  ) 

			   )

			 )
			nil
			nil
		   )

		  ( type 'CODEType'
			nil nil
			nil
			nil
			false false false
			CODEElement nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'String'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Date'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Time'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Number'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Boolean'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Text'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'SpanishDate'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		 )
		nil
	   )

	  ( module 'Aspectos'
		nil nil
		(definedAspects
		  ( aspect 'PresentationText'
			nil nil
		   )

		 )
		nil
		nil nil
		nil
		nil
	   )

	  ( module 'mda'
		nil nil
		nil
		nil
		nil nil
		nil
		(submodules
		  ( module 'mof'
			nil nil
			nil
			nil
			nil nil
			(types
			  ( type 'ModelElement'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'name'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'NameType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'annotation'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'AnnotationType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'qualifiedName'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false false false false false
					ALWAYS
					''
					''
					''
					'calc container qualifiedNameWithSeparator , "::" , name'
					''
					 ( refToType 'NameType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'qualifiedNameWithSeparator'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					ALWAYS
					''
					''
					''
					'calc qualifiedName , "::"'
					''
					 ( refToType 'NameType' 'mda' 'mof'  ) 

				   )

				 )
				(relationships
				  ( relationship 'container'
					nil nil
					nil
					nil
					isAGGREGATED nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'contents'  ( refToType 'Namespace' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'tag'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'modelElement'  ( refToType 'Tag' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'NameType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false true false false
				nil
				(supertypes
				   ( refToType 'String' 'PrimitiveTypes'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'AnnotationType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false true false false
				nil
				(supertypes
				   ( refToType 'String' 'PrimitiveTypes'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'Namespace'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'contents'
					nil nil
					nil
					nil
					AGGREGATES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'container'  ( refToType 'ModelElement' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'importer'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'imported'  ( refToType 'Import' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'GeneralizableElement'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Namespace' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'supertype'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'subtype'  ( refToType 'GeneralizableElement' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'subtype'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'supertype'  ( refToType 'GeneralizableElement' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Package'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'GeneralizableElement' 'mda' 'mof'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'Classifier'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'GeneralizableElement' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'typedElement'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'type'  ( refToType 'TypedElement' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Import'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'visibility'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal public'
					''
					 ( refToType 'VisibilityKind' 'mda' 'mof'  ) 

				   )

				 )
				(relationships
				  ( relationship 'imported'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'importer'  ( refToType 'Namespace' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'VisibilityKind'
				nil nil
				nil
				nil
				false true false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'public'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'protected'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'private'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'EvaluationKind'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'immediate'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'deferred'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'Constraint'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'expression'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'language'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'evaluationPolicy'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal immediate'
					''
					 ( refToType 'EvaluationKind' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Tag'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'tagId'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'String' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'values'
					nil nil
					nil
					nil
					'' nil
					#'0' #*
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

				   )

				 )
				(relationships
				  ( relationship 'modelElement'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'tag'  ( refToType 'ModelElement' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'TypedElement'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				nil
				nil
				(relationships
				  ( relationship 'type'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'typedElement'  ( refToType 'Classifier' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Association'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'isDerived'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false false false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal false'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'TypeDescriptor'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				nil
				nil
				nil
			   )

			  ( type 'DataType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'typeCode'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'TypeDescriptor' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Feature'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'ModelElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'visibility'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal public'
					''
					 ( refToType 'VisibilityKind' 'mda' 'mof'  ) 

				   )

				  ( attribute 'scope'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal instance'
					''
					 ( refToType 'ScopeKind' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'ScopeKind'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'static'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'instance'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'BehavioralFeature'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Namespace' 'mda' 'mof'  ) 
				   ( refToType 'Feature' 'mda' 'mof'  ) 
				 )
				nil
				nil
				nil
			   )

			  ( type 'Operation'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'BehavioralFeature' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'isQuery'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				(relationships
				  ( relationship 'exception'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'operation'  ( refToType 'MofException' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'MofException'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'BehavioralFeature' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'operation'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'exception'  ( refToType 'Operation' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'MofAttribute'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'StructuralFeature' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'is_derived'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal false'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'StructuralFeature'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Feature' 'mda' 'mof'  ) 
				   ( refToType 'TypedElement' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'multiplicity'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false true true
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'MultiplicityType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'is_changeable'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'MultiplicityType'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				(attributes
				  ( attribute 'lower'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal 1'
					''
					 ( refToType 'Integer' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'upper'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal 1'
					''
					 ( refToType 'Integer' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'is_ordered'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'is_unique'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Reference'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'StructuralFeature' 'mda' 'mof'  ) 
				 )
				nil
				(relationships
				  ( relationship 'referencedEnd'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false false false
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'referent'  ( refToType 'AssociationEnd' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'exposedEnd'
					nil nil
					nil
					nil
					REFERENCES nil
					#'1' #'1'
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'referer'  ( refToType 'AssociationEnd' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'Constant'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'constValue'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					 ( refToType 'CMGO' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'TypeAlias'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'multiplicity'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false true true
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'MultiplicityType' 'mda' 'mof'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'AssociationEnd'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				nil
				(attributes
				  ( attribute 'multiplicity'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false true true
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					''
					''
					 ( refToType 'MultiplicityType' 'mda' 'mof'  ) 

				   )

				  ( attribute 'aggregation'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal none'
					''
					 ( refToType 'AggregationKind' 'mda' 'mof'  ) 

				   )

				  ( attribute 'is_navigable'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				  ( attribute 'is_changeable'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal true'
					''
					nil
				   )

				 )
				(relationships
				  ( relationship 'referent'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'referencedEnd'  ( refToType 'Reference' 'mda' 'mof'  )  ) 
				   )

				  ( relationship 'referer'
					nil nil
					nil
					nil
					REFERENCES nil
					#'0' #*
					false false true false false true true
					NOCOMPUTATION
					''
					''
					''
					''
					( refToInverseRelationship 'exposedEnd'  ( refToType 'Reference' 'mda' 'mof'  )  ) 
				   )

				 )
				nil
			   )

			  ( type 'AggregationKind'
				nil nil
				nil
				nil
				true false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'CMGO' 'PrimitiveTypes'  ) 
				 )
				(attributes
				  ( attribute 'none'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'shared'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				  ( attribute 'composite'
					nil nil
					nil
					nil
					'' nil
					#'0' #'1'
					false false false false true false true true
					NOCOMPUTATION
					''
					''
					''
					''
					''
					nil
				   )

				 )
				nil
				nil
			   )

			  ( type 'Class'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false false false
				nil
				(supertypes
				   ( refToType 'Classifier' 'mda' 'mof'  ) 
				 )
				(attributes
				  ( attribute 'is_singleton'
					nil nil
					nil
					nil
					'' nil
					#'1' #'1'
					false false false false true false false false
					INITIALIZEDINCONSTRUCTOR
					''
					''
					''
					'literal false'
					''
					 ( refToType 'Boolean' 'PrimitiveTypes'  ) 

				   )

				 )
				nil
				nil
			   )

			  ( type 'Model'
				nil nil
				nil
				nil
				false false false
				nil nil
				nil
				false false true false
				nil
				(supertypes
				   ( refToType 'Package' 'mda' 'mof'  ) 
				 )
				nil
				nil
				nil
			   )

			 )
			nil
		   )

		 )
	   )

	 )
	CMAPTranslationHolder cmapNoTranslationStore
   )! !

!CMAPPathFinder class publicMethodsFor: 'class initialization'!

initialize
	"CMAPPathFinder initialize.
	CMAPPathFinder allSubclasses do: [:aClass | aClass initialize]"


	super initialize! !

!CMAPPathFinder class publicMethodsFor: 'generated resources'!

littleCloseIconAndLabel

	^(LabelAndIcon with: (self nlsIconLabel: 'Cerrar') asText allBold) icon: self littleCloseIcon! !

!CMAPPathFinder class publicMethodsFor: 'instance creation'!

browserKind
	^#CMAPPathFinder!

checkedBrowserParameters:	theBrowserParameters
	| aDict |

	aDict := theBrowserParameters isNil ifTrue: [ Dictionary new] ifFalse: [ theBrowserParameters].

	self in: aDict at: METABrowser showCanvasLabelParameterSymbol 			ifAbsentPut: true.

	aDict at: METABrowser editorsOpenerParameterSymbol put: CMAPEditorsOpener editorsOpener.
	self in: aDict at: METABrowser numberOfEditorHoldersParameterSymbol 	ifAbsentPut: 3.

	aDict at: METABrowser showClassNodePrefixesInPathFinderSymbol put:
		(self getMETAConfigurationParameterValue: #showClassNodePrefixesInPathFinder default: false).
	
	^super checkedBrowserParameters:	theBrowserParameters.! !

!CMAPPathFinder class publicMethodsFor: 'interface specs'!

aboutSpec
	^OMGCMAPLauncherPanel autorCanvasSpec!

technicalSupportSpec

	^OMGCMAPLauncherPanel soporteCanvasSpec!

windowSpec


	"UIPainter new openOnClass: self andSelector: #windowSpec"

	^#(#FullSpec #window: #(#WindowSpec #label: 'Browser' #min: #(#Point 400 300 ) #bounds: #(#Rectangle 32 32 932 732 ) #flags: 4 #menu: #metaApplicationBrowserMenu ) #component: #(#SpecCollection #collection: #( ) ) )! !

!CMAPPathFinder class publicMethodsFor: 'menu'!

assistantDebugMenu
	"self assistantDebugMenu startUp"

	^super assistantDebugMenu!

assistantRuntimeMenu
	"self assistantRuntimeMenu startUp"

	^super assistantDebugMenu!

helpMenu
	"self helpMenu startUp"

	^#(#HWHelpPopUpMenu #('_Help' '_Author' '_Support') #(1 2 ) #( #help #about #techSupport) #('Get help on OMGCMAPware' 'About Author of OMGCMAPware' 'Support on OMGCMAPware') ) decodeAsLiteralArray!

listWindowsMenu
	"CMAPPathFinder listWindowsMenu startUp"

	^RTDynamicHelpPopUpMenu fromOptionsBlock: 
		(RTDynamicHelpPopUpMenu windowsRaiseBuildBlockGetWindowsFromBlock: self getWindowsBlock)
		helpString: self windowsRaiseHelpString!

metaApplicationBrowserDebugMenu
	"self metaApplicationBrowserDebugMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Browser' 'Path' 'Layout' '-- not used --' '_Windows' '_Help' 'Developer') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Browser' 'Path' 'Layout' '-- not used --' '_Windows' 'Herramientas' '_Help' 'Developer') copy
				lines: #()
				values: (OrderedCollection new 
					add: self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					add: self itselfMenu;
					yourself)
				helps: #(''  '' '' '' '' '' ''  '')].


	^aMenu!

metaApplicationBrowserMenu
	^DEBUGDvpt
		ifTrue: [ self metaApplicationBrowserDebugMenu]
		ifFalse: [ self metaApplicationBrowserRuntimeMenu]!

metaApplicationBrowserRuntimeMenu
	"self metaApplicationBrowserRuntimeMenu startUp"

	| aMenu aToolsMenu |

	aToolsMenu := self toolsMenu.
	aToolsMenu isNil
		ifTrue: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Browser'  'Path' 'Layout' '-- not used --' '_Windows' '_Help') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: self helpMenu;
					yourself)
				helps: #('' '' ''  '' '' '')]
		ifFalse: [
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_Browser' 'Path'  'Layout' '-- not used --' '_Windows' 'Tools' '_Help') copy
				lines: #()
				values: (OrderedCollection new 
					add:  self browserMenu;
					add: self pathMenu;
					add: self viewsMenu;
					add: self assistantMenu;
					add: self windowsMenu;
					add: aToolsMenu;
					add: self helpMenu;
					yourself)
				helps: #('' ''  '' '' '' '' ''  )].

	^aMenu!

pathMenu
	"self pathMenu startUp"

	^#(#HWHelpPopUpMenu #('_From Start' '_Back' '_Open Path Window' '_Close Path Window' ) #(2 ) #(#goToFirstInHistory #backOneInHistory #showPathWindow #hidePathWindow ) #('Show first object' 'Show previous object' 'Open Path Window' 'Close Path Window' ))  decodeAsLiteralArray!

viewsMenu
	"self viewsMenu startUp"

	^#(#HWHelpPopUpMenu #('_Browser only' 'Editor only' 'Browser and Editor' '-- not used --' ) #(3 ) #(#onlyDisplayLists #onlyDisplayEditor #displayListAndEditor #showInheritedMode ) #( 'Show only the Browser pane' 'Show only the Editor pane' 'SHow both Browser and Editor panes' '-- not used --' ) )  decodeAsLiteralArray!

windowsMenu
	"self windowsMenu startUp"
	^self listWindowsMenu!

windowsRaiseHelpString
	^'Show Window ' copy! !

!CMAPPathFinder class publicMethodsFor: 'ref:preferences'!

preferredConfigurationsBrowserClass
	^CMAPConfigurationsBrowser!

preferredEditorsOpenerClass
	^CMAPEditorsOpener!

preferredMETAConfigurationClass

	^CMAPMETAConfiguration! !

!CMAPPathFinder publicMethodsFor: 'error handling'!

logControlLoopException: theException!

notifyControlLoopException: theException

	Dialog warn: 'Ha ocurrido un error.\Su ventana ha sido cerrada.' withCRs! !

!CMAPPathFinder publicMethodsFor: 'label'!

browserLabelTitlePrefix
	^'OMGCMAPware: ' copy!

numberOfEditorHolders
	| aBrowserParameters aNumEditorHolders anObjectHolder |

	anObjectHolder := self objectHolder.
	anObjectHolder isNil ifFalse: [ 
		aBrowserParameters := anObjectHolder browserParameters.
		aBrowserParameters isNil ifFalse: [ 
			aNumEditorHolders := aBrowserParameters at: METABrowser numberOfEditorHoldersParameterSymbol ifAbsent: [ nil].
			aNumEditorHolders isNil ifFalse: [ ^aNumEditorHolders]]].


	 ^super numberOfEditorHolders "delegate on default way of finding it : through class method on configuration"! !

!CMAPPathFinder publicMethodsFor: 'menu'!

about
	OMGCMAPLauncherPanel openWithSpec: #autorWindowSpec!

canRecord
	^false!

help

	Dialog warn: 
		'Por favor, contacte Soporte tecnico\para realizar cualquier consulta\y recibir un manual de instrucciones'
		withCRs.
	self techSupport!

techSupport
	OMGCMAPLauncherPanel openWithSpec: #soporteWindowSpec! !

!CMAPPathFinder publicMethodsFor: 'updating'!

updateWindowLabel

	| aLabel aBrowserParameters aPrefix aPrefixString |
	aLabel :=  (self readOnly 
		ifTrue: ['Solo Lectura ', self browserLabelTitlePrefix] 
		ifFalse: [self browserLabelTitlePrefix]),
		objectHolder getValueClassName , ' ', objectHolder getValueName.

	aBrowserParameters := objectHolder browserParameters.

	aPrefix := aBrowserParameters isNil
		ifTrue: ['']
		ifFalse: [ 
			aPrefixString := aBrowserParameters at: METABrowser windowLabelPrefixSymbol ifAbsent: [nil].
			aPrefixString isNil
				ifTrue: ['']
				ifFalse: [aPrefixString, ' ']].

	
	self builder window label:  aPrefix , aLabel.
	self isDialog ifTrue: [ self dialogLabel value: 'Selection ', aPrefix , aLabel].

	self pathListApplication isNil ifFalse: [self pathListApplication updateWindowLabel].!

updateWindowsList
 
	self updateWindowsListMenu!

updateWindowsListMenu

	| aMenu aWindowsListMenu aMenuHolder |

	aMenuHolder := self metaApplicationBrowserMenu value.
	aMenuHolder isNil ifTrue: [ ^self].

	aMenu := aMenuHolder value.
	aMenu isNil ifTrue: [ ^self].

	aWindowsListMenu := aMenu valueAt: self class windowsSubMenuIndexInMenu.
	aWindowsListMenu buildMenu.! !

!CMAPPilot class publicMethodsFor: 'button indexes accessing'!

newProjectButtonIndex
	^2!

openProjectButtonIndex
	^3!

saveProjectButtonIndex
	^4! !

!CMAPPilot class publicMethodsFor: 'class initializing'!

buildButtonsBarFromVisuals: symbols andActions: selectors

	| aMenu help |
	buttonsBar := OrderedCollection new.
	symbols with: selectors do: [:symbol :action | 
		buttonsBar add: ((HWActionBarButton new)
						visual: (self visualIconNamed: symbol);
						 model: action)].
	(30 - buttonsBar size) timesRepeat: [ 
		buttonsBar add: ((HWActionBarButton new)
						visual: (self visualIconNamed: #space);
						 model: #yourself) ].
	aMenu := self menuBar asDictionaryValueHelp.
	buttonsBar do:  [:each | 
		help := aMenu at: each model ifAbsent: [''].
		each help: help]
"*VIPVersion  'Anonymous Version from Changes File'*"!

buildMultiButtonsBarFromVisuals: symbols andActions: selectors
 
	| aMenu help | 
	multiButtonsBar := OrderedCollection new.
	symbols with: selectors do: [:symbol :action | 
		multiButtonsBar add: ((HWActionBarButton new)
						visual: (self visualIconNamed: symbol);
						 model: action)].
	(30 - buttonsBar size) timesRepeat: [ 
		multiButtonsBar add: ((HWActionBarButton new)
						visual: (self visualIconNamed: #space);
						 model: #yourself) ].
	aMenu := self menuBar asDictionaryValueHelp.
	multiButtonsBar do:  [:each | 
		help := aMenu at: each model ifAbsent: [''].
		each help: help]
"*VIPVersion 3-5-96 | 2:03:57 pm 'ACV'*"!

initialize
	"CMAPPilot initialize"

	self initializeSpecificVisualIcons.
	self initializeButtonsBar!

initializeButtonsBar

	"self initializeSpecificVisualIcons. self initializeButtonsBar"

	self	buildButtonsBarFromVisuals:  
		#(#close #space
			#new #open #save #space 
			#questionMark #space
			#refresh)
		andActions:  #(#quit #yourself
			#newProjectMono #openProjectMono #saveProjectMono #yourself 
			#help #yourself
			#refreshWindow).

	self	buildMultiButtonsBarFromVisuals:  
		#(#close #space
			#new #open #save #space 
			#questionMark #space
			#refresh)
		andActions:  #(#quit #yourself
			#newProjectMulti #openProjectMulti #commitMulti #yourself 
			#help #yourself
			#refreshWindow)!

initializeSpecificVisualIcons

	specificVisualIcons := HWOrderedDictionary new.
	specificVisualIcons at: #refresh put: (HWUIButtonsBarBuilder visualIconNamed: #refresh)! !

!CMAPPilot class publicMethodsFor: 'constants'!

saveDefaultPathFileName
	^'Model_CMAPware.cmap' copy! !

!CMAPPilot class publicMethodsFor: 'generated resources'!

littleQuitIconAndLabel
	^(LabelAndIcon with: 'Close' asText allBold) icon: self littleQuitIcon! !

!CMAPPilot class publicMethodsFor: 'interface specs'!

aboutSpec
	^CMAPLauncherPanel autorCanvasSpec!

technicalSupportSpec

	^CMAPLauncherPanel soporteCanvasSpec!

windowSpec
	"UIPainter new openOnClass: self andSelector: #windowSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware menu' 
			#min: #(#Point 487 236 ) 
			#bounds: #(#Rectangle 255 248 833 632 ) 
			#flags: 4 
			#menu: #menuBar 
			#colors: 
			#(#LookPreferences 
				#setForegroundColor: #(#ColorValue #black ) 
				#setBackgroundColor: #(#ColorValue #white ) 
				#setSelectionForegroundColor: #(#ColorValue #white ) 
				#setSelectionBackgroundColor: #(#ColorValue #black ) 
				#setBorderColor: #(#ColorValue #black ) ) ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#RegionSpec 
					#layout: #(#LayoutFrame 0 0 0 0 0 1 0 1 ) 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue 6553 6553 6553 ) ) ) 
				#(#OSPCardFileSpec 
					#layout: #(#LayoutFrame 5 0 35 0 -5 1 -35 1 ) 
					#name: #panelsTabsID 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue #white ) ) 
					#model: #panelsTabsModel 
					#tabable: true 
					#binderSide: #bottom 
					#binderWidth: 0 
					#majorTabsOnRight: false 
					#rightInset: 2 
					#bottomInset: 20 
					#orientation: #bottomTopNone 
					#tabEnd: #beveledTab 
					#buttonLocation: 'Lower Right' 
					#verticalButtons: false 
					#horizontalButtons: false 
					#cached: true 
					#cacheSize: 2 
					#cacheModel: #panelsModel 
					#scrollTabs: false 
					#tabsAcross: 2 
					#layeredIndent: false ) 
				#(#InputFieldSpec 
					#layout: #(#LayoutFrame 5 0 -30 1 -95 1 -5 1 ) 
					#name: #HelpHolderID 
					#colors: 
					#(#LookPreferences 
						#setForegroundColor: #(#ColorValue #black ) 
						#setBackgroundColor: #(#ColorValue 6553 6553 6553 ) ) 
					#model: #helpHolder 
					#tabable: false 
					#style: #default 
					#isReadOnly: true ) 
				#(#ActionButtonSpec 
					#layout: #(#LayoutFrame -90 1 -30 1 -5 1 -5 1 ) 
					#name: #CloseButtonID 
					#model: #quit 
					#label: #littleQuitIconAndLabel 
					#hasCharacterOrientedLabel: false ) ) ) )! !

!CMAPPilot class publicMethodsFor: 'menu accessing'!

ayudaMenuIndex
	^4!

cmapWareMenuIndex
	^1!

modelMenuIndex
	^2!

sistemaMenuIndex
	^3! !

!CMAPPilot class publicMethodsFor: 'mono menu accessing'!

cmapWareChooseApplicationMonoMenuItemIndex
	^1!

cmapWareCloseMonoMenuItemIndex
	^8!

cmapWareLoginLogoutMonoMenuItemIndex
	^2!

cmapWareNewMonoMenuItemIndex
	^3!

cmapWareOpenMonoMenuItemIndex
	^4!

cmapWarePostChooseApplicationMonoMenuItemIndexes
	^Array 
		with: self cmapWareLoginLogoutMonoMenuItemIndex!

cmapWarePostLoginMonoMenuItemIndexes
	^Array 
		with: self cmapWareNewMonoMenuItemIndex
		with: self cmapWareOpenMonoMenuItemIndex!

cmapWarePostOpenMonoMenuItemIndexes
	^Array 
		with: self cmapWareSaveMonoMenuItemIndex
		with: self cmapWareSaveAsMonoMenuItemIndex
		with: self cmapWareCloseMonoMenuItemIndex!

cmapWarePreChooseApplicationMonoMenuItemIndexes
	^Array with: self cmapWareChooseApplicationMonoMenuItemIndex!

cmapWarePreLoginMonoMenuItemIndexes
	^Array with: self cmapWareLoginLogoutMonoMenuItemIndex!

cmapWareQuitMonoMenuItemIndex
	^9!

cmapWareSaveAsMonoMenuItemIndex
	^6!

cmapWareSaveMonoMenuItemIndex
	^5!

cmapWareSnapshotMonoMenuItemIndex
	^7!

loginMonoMenuItemHelp
	^'Connect User to a Session in CMAPware' copy!

loginMonoMenuItemLabel
	^'Connect'!

loginMonoMenuItemSelector
	^#login!

logoutMonoMenuItemHelp
	^'Disconnect User from session in CMAPware'!

logoutMonoMenuItemLabel
	^'Disconnect' copy!

logoutMonoMenuItemSelector
	^#logout!

postLoginMonoMenuItemIndexes
	^Array new!

postOpenMonoMenuItemIndexes
	^Array 
		with: self modelMenuIndex!

sistemaConfiguracionItemIndex
	^2!

sistemaInspectItemIndex
	^2!

sistemaOpenCMAPModelBrowserItemIndex
	^3!

sistemaOpenModelAppBrowserItemIndex
	^2!

sistemaOpenVisualLauncherItemIndex
	^4!

sistemaPostLoginMonoMenuItemIndexes
	^Array 
		with: self sistemaOpenCMAPModelBrowserItemIndex
		with: self sistemaOpenVisualLauncherItemIndex
		with: self sistemaResetAllCurrentTranslationsModelsAndInfosItemIndex
		with: self sistemaInspectItemIndex!

sistemaPostOpenMonoMenuItemIndexes
	^Array 
		with: self sistemaOpenModelAppBrowserItemIndex!

sistemaPreLoginMonoMenuItemIndexes
	^Array 
		with: self sistemaSnapshotMonoMenuItemIndex!

sistemaResetAllCurrentTranslationsModelsAndInfosItemIndex
	^4!

sistemaSnapshotMonoMenuItemIndex
	^1! !

!CMAPPilot class publicMethodsFor: 'opening'!

inactiveDefaultPilotWith: aTextCollector
	"Create the Pilot using aTextCollector but don't open it.  Answer the window."

	| aPilot aWindow |
	aPilot :=self new.
	aWindow := (aPilot allButOpenInterface: #windowSpec) window.
	aWindow moveTo: 20@60.
	^aWindow! !

!CMAPPilot class publicMethodsFor: 'preferences'!

preferredApplicationConfigurationsCollectionClass

	^CMAPApplicationConfigurationsCollection!

preferredConfigurationsBrowserClass

	^CMAPConfigurationsBrowser! !

!CMAPPilot class publicMethodsFor: 'preferences-mngmnt'!

loadPreferences
	"IAALP2PIlot loadPreferences"

	| bos prefs |
	'ibee.prf' asFilename exists ifFalse: [^nil].
	self errorSignal
		handle: 
			[:ex | 
			OMTDialog warn: 'Corrupted Preferences file' asLPString.
			InputState default shiftDown ifTrue: [self error: 'Bad File !!']]
		do: 
			[bos := BinaryObjectStorage onOld: 'ibee.prf' asFilename readStream.
			[prefs := bos next]
				valueNowOrOnUnwindDo: [bos close].
			prefs do: [:each | each apply]]
"*VIPVersion  'Anonymous Version from Changes File'*"!

savePreferences
	"IAALP2Pilot savePreferences"
	| prefs bos |
	prefs := #(  HWPrinterPreferences )
				collect: [:cn | (Smalltalk at: cn) new read].

	bos := BinaryObjectStorage onNew: 'ibee.prf' asFilename writeStream.
	bos sourceMode: #discard.
	[bos nextPut: prefs ]
		valueNowOrOnUnwindDo: [ bos close ]
"*VIPVersion  'Anonymous Version from Changes File'*"! !

!CMAPPilot class publicMethodsFor: 'private'!

copyMenuFrom: theMenu

	^theMenu copy! !

!CMAPPilot class publicMethodsFor: 'resources'!

ayudaMenu

	| aMenu |
	aMenu := HWHelpPopUpMenu 
		labelList: #(
			('_Support' ) ( '_Author' ) ('_Help'))
		values:  #(
			#openSoporteNotice
			#openAuthorNotice
			#help
		).

	aMenu helps: #(
		'Show info to obtain support for CMAPware'
		'Show Copyright notice for CMAPware'
		'Show help for CMAPware'
	).

	^aMenu!

buttonsBar

	^buttonsBar!

cmapWareMenuMono

	| aMenu |
	aMenu := HWHelpPopUpMenu 
		labelList: #(
			('_Application...')
			('_Connect...')
			('_New Model' ) ( '_Open Model...' '_Save' 'Save _As...' 'Snapshot') 
			( 'Close')
			('Exit'))
		values:  #(
			#chooseApplication
			#login
			#newProject
			#openProject
			#saveProject
			#saveAsProject
			#snapshot
			#closeProject
			#quit ).

	aMenu helps: #(
		'Choose a CMAPware Application to run'
		'Connect Uset to a Session in CMAPware'
		'Create new Model in CMAPware'
		'Read Model file into CMAPware'
		'Write current Model into a CMAPware file'
		'Write current Model into a different CMAPware file'
		'Write a snapshot of the complete current state of CMAPware'
		'Close current Model in CMAPware'
		'Exit CMAPWare' ).

	^aMenu!

itselfMenu 
	^#(#HWHelpPopUpMenu #('Itself' ) #() #(#(#HWHelpPopUpMenu #('Inspeccionar' ) #( ) #(#inspectItself  ) #(''  ) ) ) #('' ) ) decodeAsLiteralArray!

menuBar
	| aMenu |
	
			aMenu := HWHelpPopUpMenu 
				labelArray: #('_CMAPWare'  '_Model' '_System' '_Help')  copy
				lines: #()
				values: (OrderedCollection new  
					add: self cmapWareMenuMono;
					add: self modelMenu;
					add: self sistemaMenu;
					add: self ayudaMenu;
					yourself)
				helps: #('' ).

	^aMenu!

modelMenu

	| aMenu |
	aMenu := HWHelpPopUpMenu 
		labelList: #(
			('_Model Browser') )
		values:  #( 
			openCurrentModel
		).

	aMenu helps: #(
		'Browse the Model in CMAPware'
	).

	^aMenu!

sistemaMenu

	| aMenu |
	aMenu := HWHelpPopUpMenu 
		labelList: #(
			('_Configs')
			('_Domain'  '_Metamodel '  '_Programming' '_Inspect') 
			( 'Clear'))
		values:  #(
			#openBrowsersConfigurationsBrowser
			#openDomainBrowser
			#openMetaModelBrowser
			#openVisualLauncher
			#inspectItself
			#resetAllCurrentTranslationsModelsAndInfos

		).

	aMenu helps: #(
		'Browse Configuration parameters controlling behavior and presentation of CMAPware'
		'Browse the whole Object Domain of instances of current Model in CMAPware'
		'Browse the Metamodel in CMAPware'
		'Open programmers launcher'
		'Inspect Menu window in CMAPware'
		'Clear all information and metamodel changes in CMAPware (fully destroying your last unsaved changes!!)'
	).

	^aMenu! !

!CMAPPilot class publicMethodsFor: 'snapshot'!

justReturnedFromSnapshot
	^justReturnedFromSnapshot == true!

justReturnedFromSnapshot: theValue
	justReturnedFromSnapshot := theValue == true!

removeDanglingDependents

	CODEElement huntInDependents!

systemToCleanState
	ObjectMemory verboseGlobalCompactingGC.! !

!CMAPPilot class publicMethodsFor: 'specificvisualIcons'!

specificVisuals

	specificVisualIcons == nil ifTrue: [self initializeSpecificVisualIcons].
	^specificVisualIcons!

visualIconNamed: aSymbol

	| visual |
	visual := HWUIButtonsBarBuilder visualIconNamed: aSymbol.
	visual == nil ifFalse: [^visual].
	Screen default colorPalette isMonochrome
		ifTrue: [ ^(self specificVisuals at: aSymbol) first ]
		ifFalse: [ ^(self specificVisuals at: aSymbol) last ]! !

!CMAPPilot class publicMethodsFor: 'version'!

collaborators

	^'CMAPWARE.\Antonio Carrasco-Valero, Author.' withCRs asValue!

versionText

	^'CMAP	version :  0.8' asText allBold! !

!CMAPPilot publicMethodsFor: 'accessing'!

collaborators


	^self class collaborators!

defaultPath
	^defaultPath!

defaultPath: thePath
	| aPath |
	aPath := thePath isNil ifTrue: [ '']  ifFalse: [ thePath].

	defaultPath := aPath.
	self saveDefaultPath!

helpFileNames
	| col |
	col := OrderedCollection new.
	col add: ('CMAP' -> 'cmap.hlp').
	^col asArray!

isLogged

	^self logged value == true!

logged

	| aManager |
	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].
	^aManager logged!

manager
	manager isNil ifTrue: [ self initManager].
	^manager!

saveDefaultPath
	| aPath aSaveDefaultPathFileName aStream |
	aPath := self defaultPath.
	aPath isNil ifTrue: [ ^self].

	aSaveDefaultPathFileName := self saveDefaultPathFileName.
	aSaveDefaultPathFileName isNil ifTrue: [ ^self].

	[
		aStream := aSaveDefaultPathFileName asFilename writeStream.
		aStream nextPutAll: aPath; cr
	] valueNowOrOnUnwindDo: [ aStream isNil ifFalse: [ aStream close]]!

userName
	^userName!

userName: theUserName
	userName := theUserName!

versionText
	^self class versionText asValue! !

!CMAPPilot publicMethodsFor: 'actions'!

about
	CMAPLauncherPanel openWithSpec: #autorCanvasSpec!

closeProject
	| aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].

	aManager isLogged ifFalse: [ ^nil].

	[ 
		self panelsTabsModel selectionIndex: 1.
		self panelsTabsModel selectionIndex: 2.
	
		aManager closeProject

	] valueNowOrOnUnwindDo: [ 
		(Delay forMilliseconds: 500) wait.
		self panelsTabsModel selectionIndex: 2.
		self panelsTabsModel selectionIndex: 1.
	].

	self updateMenuAndButtonsBar.!

help

	Dialog warn: 
		'Please, refer to user documentation, or contact technical support.'
		withCRs.
	self techSupport!

login
	| aUserName anApplicationConfiguration anApplicationName aManager aNumAttempts |

	aManager := self manager.
	aManager isNil ifTrue: [ ^false].

	aManager isLogged ifTrue: [ ^false].

	anApplicationConfiguration := aManager applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^false].

	anApplicationName := anApplicationConfiguration applicationName.
	(anApplicationName isNil or: [ anApplicationName isEmpty])  ifTrue: [ ^false].

	aNumAttempts := 0.
	[
		aNumAttempts := aNumAttempts + 1.

		aUserName := Dialog request: 'Please, enter your login name for ' , anApplicationName  initialAnswer: String new.
		(aUserName isNil not and: [ aUserName isEmpty not]) ifTrue:  [ 
			aUserName := aUserName trimBlanks.
			aUserName isEmpty ifFalse:  [ 
				aManager login: aUserName
			]
		].
		aManager isLogged not and: [ aNumAttempts < self class maxLoginAttempts]
	] whileTrue.


	aManager isLogged ifTrue: [  
		self uiMessage: 'User ' , (aUserName isNil ifTrue: [ '?'] ifFalse: [ aUserName]), 
				' Connected on ' , (Date today printString), ' ', (Time now printString).
	].

	^aManager isLogged!

logout
	
	| anApplicationConfiguration anApplicationName aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^true].

	aManager isLogged ifFalse: [ ^true].

	anApplicationConfiguration := aManager applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^true].

	anApplicationName := anApplicationConfiguration applicationName.
	(anApplicationName isNil or: [ anApplicationName isEmpty])  ifTrue: [ ^true].

	(self uiConfirm: 'Do you really want to disconnect from ' , anApplicationName , ' ?' initialAnswer: false) ifFalse: [^false].

	aManager logout.

	^true!

newProject
	| aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].

	aManager isLogged ifFalse: [ ^nil].

	[ 
		self panelsTabsModel selectionIndex: 1.
		self panelsTabsModel selectionIndex: 2.
	
		aManager newProject

	] valueNowOrOnUnwindDo: [ 
		(Delay forMilliseconds: 500) wait.
		self panelsTabsModel selectionIndex: 2.
		self panelsTabsModel selectionIndex: 1.
	].

	self updateMenuAndButtonsBar.!

openAuthorNotice

	CMAPLauncherPanel openWithSpec: #authorWindowSpec!

openBrowsersConfigurationsBrowser
	
	CMAPConfigurationsBrowser open!

openConfiguration


Cursor wait showWhile: [ 
	self preferredConfigurationsBrowserClass open
]!

openCurrentModel

	| anApplicationConfiguration aInfoHolderClass aManager |
	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].
	anApplicationConfiguration := aManager applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aInfoHolderClass := anApplicationConfiguration infoHolderClass.
	aInfoHolderClass isNil ifTrue: [ ^nil].

	Cursor wait showWhile: [ 
		aInfoHolderClass browseCurrentModelWIthApplicationConfiguration: anApplicationConfiguration
	]!

openDeveloperConfigurationsBrowser
	
Cursor wait showWhile: [ 
	CMAPConfigurationsBrowser open
]!

openDomainBrowser

	| anApplicationConfiguration aInfoHolderClass aInfoStoreMethodSelector |
	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aInfoHolderClass := anApplicationConfiguration infoHolderClass.
	aInfoHolderClass isNil ifTrue: [ ^nil].

	aInfoStoreMethodSelector := anApplicationConfiguration infoStoreMethodSelector.
	aInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	Cursor wait showWhile: [ 
		aInfoHolderClass browseModelStoreMetodSelector:aInfoStoreMethodSelector withApplicationConfiguration: anApplicationConfiguration
	]!

openMetaModelBrowser

	| anApplicationConfiguration aMetaInfoHolderClass aMetaInfoStoreMethodSelector |
	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aMetaInfoHolderClass := anApplicationConfiguration metaInfoHolderClass.
	aMetaInfoHolderClass isNil ifTrue: [ ^nil].

	aMetaInfoStoreMethodSelector := anApplicationConfiguration metaInfoStoreMethodSelector.
	aMetaInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	Cursor wait showWhile: [ 
		aMetaInfoHolderClass browseMetaModelStoreMetodSelector:aMetaInfoStoreMethodSelector withApplicationConfiguration: anApplicationConfiguration
	]!

openProject
	| aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].

	aManager isLogged ifFalse: [ ^nil].

	[ 
		self panelsTabsModel selectionIndex: 1.
		self panelsTabsModel selectionIndex: 2.
	
		aManager openProject

	] valueNowOrOnUnwindDo: [ 
		(Delay forMilliseconds: 500) wait.
		self panelsTabsModel selectionIndex: 2.
		self panelsTabsModel selectionIndex: 1.
	].

	self updateMenuAndButtonsBar.!

openQuickAssistantBrowser!

openSoporteNotice

	CMAPLauncherPanel openWithSpec: #soporteWindowSpec!

openSupportNotice

	CMAPLauncherPanel openWithSpec: #supportWindowSpec!

openVisualLauncher
	
	VisualLauncher open!

quit

	(Dialog confirm: 'Desea ABANDONAR CMAPware ?' withCRs initialAnswer: false)  ifFalse: [ ^self].
	(Dialog confirm: 'Realmente desea ABANDONAR CMAPware ?'  withCRs initialAnswer: false)  ifFalse: [ ^self].

	self closeRequest.
	
	DEBUGDvpt ifFalse: [ ObjectMemory quit]!

refreshWindow
	builder window controller display!

resetAllCurrentTranslationsModelsAndInfos

	(Dialog confirm: ('Desea ELIMINAR toda la informacion en CMAPware\',
		'y devolver el Modelo y traducciones al estado original ?')withCRs initialAnswer: false)  ifFalse: [ ^self].
	
	(Dialog confirm: ('Realmente desea ELIMINAR toda la informacion en CMAPware\',
		'y devolver el Modelo y traducciones al estado original ?' ) withCRs initialAnswer: false)  ifFalse: [ ^self].
	

Cursor wait showWhile: [ 
	TranslationsPersistencyHolder resetAllCurrentTranslationsNoDialog.
	CMAPMetaInfoHolder resetAllCurrentModelsNoDialog.
	CMAPInfoHolder resetAllCurrentInfosNoDialog.
]!

saveAsProject
	| aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].

	aManager isLogged ifFalse: [ ^nil].

	[ 
		self panelsTabsModel selectionIndex: 1.
		self panelsTabsModel selectionIndex: 2.
	
		aManager saveAsProject

	] valueNowOrOnUnwindDo: [ 
		(Delay forMilliseconds: 500) wait.
		self panelsTabsModel selectionIndex: 2.
		self panelsTabsModel selectionIndex: 1.
	].

	self updateMenuAndButtonsBar.!

savePreferences
	self class savePreferences!

saveProject
	| aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].

	aManager isLogged ifFalse: [ ^nil].

	[ 
		self panelsTabsModel selectionIndex: 1.
		self panelsTabsModel selectionIndex: 2.
	
		aManager saveProject

	] valueNowOrOnUnwindDo: [ 
		(Delay forMilliseconds: 500) wait.
		self panelsTabsModel selectionIndex: 2.
		self panelsTabsModel selectionIndex: 1.
	].

	self updateMenuAndButtonsBar.!

snapshot
	
	| aPrematureExitBlock aNameRoot aNameTermination aFileSystemMatchString aFileSystemFilePostfixSeparator aMatchTemplate someExistingFileNames anIndex aMaxIndex aNumber aNewIndex aNewFileName aSnapshotName aFilename aDotPosition aDate aTime aMessage |
	aPrematureExitBlock := [
		Dialog warn: 'La grabacion de "instantanea" del estado de CMAPware has sido cancelada'.
		^nil
	].

	(Dialog confirm: 
		'Realmente desea Grabar una instantanea del estado actual de CMAPware ?' initialAnswer: false) ifFalse: [ 
		aPrematureExitBlock value
	].

	aNameRoot := 'CMAPware_instantanea_'.
	aNameTermination := 'im'.
	aFileSystemMatchString := '*'.
	aFileSystemFilePostfixSeparator := '.'.

	aMatchTemplate := aNameRoot, aFileSystemMatchString, aFileSystemFilePostfixSeparator, aNameTermination.

	someExistingFileNames := Filename filesMatching: aMatchTemplate.
	anIndex := someExistingFileNames isEmpty
		ifTrue: [ 0]
		ifFalse: [ 
			someExistingFileNames size > 32 ifTrue: [ 
				Dialog warn: ('Usted ya tiene archivadas ', someExistingFileNames size printString, ' instantaneas.\',
					'Recuerde que estas instantaneas ocupan espacio en disco\',
					'La grabacion de instantanea va a continuar verificando y grabando ...') withCRs.
			].
			aMaxIndex := 0.
			someExistingFileNames do: [:aFileName |
				aNumber := Object errorSignal handle: [:anEx | anEx returnWith: 0]
					do: [ Number readFrom: (aFileName copyFrom: aNameRoot size + 1 to: aFileName size) readStream].
				aNumber > aMaxIndex ifTrue: [ aMaxIndex := aNumber].
			].
			aMaxIndex
		].

	aNewIndex := anIndex.
	[
		aNewIndex := aNewIndex + 1.
		aNewFileName := aNameRoot, aNewIndex printString, aFileSystemFilePostfixSeparator, aNameTermination.
		aNewFileName asFilename exists
	] whileTrue.
	
	[
		aSnapshotName := Dialog request: 'Por favor, introduzca un nombre de fichero instantanea a grabar' initialAnswer: aNewFileName.
		(aSnapshotName isNil or: [ aSnapshotName trimBlanks isEmpty]) ifTrue: [  aPrematureExitBlock value].
		aDotPosition := aSnapshotName indexOf: $..
		aDotPosition > 0 ifTrue: [ aSnapshotName := aSnapshotName copyFrom: 1 to: aDotPosition -1].
	
		aFilename := Object errorSignal 
			handle: [:anEx | anEx returnWith: nil]
			do: [ (aSnapshotName , aFileSystemFilePostfixSeparator, aNameTermination) asFilename].
		aFilename isNil
			ifTrue: [ 
				(Dialog 
					confirm: 'Ha introducido un nombre de fichero incorrecto.\Desea volver a intentarlo ?' withCRs
					initialAnswer: true) ifFalse: [ aPrematureExitBlock value].
				false
			]
			ifFalse: [
				aFilename exists 
					ifTrue: [ 
						(Dialog 
							confirm: 'Ha introducido un nombre de fichero ya existente.\Desea volver a intentarlo ?' withCRs
							initialAnswer: true) ifFalse: [ aPrematureExitBlock value].
						false
					]
					ifFalse: [ true]
			].
	] whileFalse.


	(Dialog confirm: 
		('CMAPware va a grabar una instantanea de su estado en el fichero \\', aSnapshotName ,
		'\ \Desea realmente grabar la instantanea ?') withCRs initialAnswer: true) ifFalse: [  aPrematureExitBlock value].

	self class justReturnedFromSnapshot: false.
	aDate := SpanishDate today.
	aTime:= Time now.

	self panelsTabsModel selectionIndex: 1.
	self panelsTabsModel selectionIndex: 2.

	ObjectMemory verboseGlobalCompactingGC.
	ObjectMemory snapshotAs: aSnapshotName thenQuit: false.

	self class justReturnedFromSnapshot
		ifTrue: [ 
			aMessage :=
				('CMAPware ha sido lanzado desde una instantanea de su estado que fue grabada el\',
				aDate printString , '  a las ' , aTime printString , '\en el fichero \\', aSnapshotName ,
				'\Todas las informaciones y ventanas se encuentran exactamente como estaban en ese momento.') withCRs.
			Dialog warn: aMessage.
			Transcript show: aMessage; cr
		]
		ifFalse:[ 
			aMessage := ('CMAPware ha grabado una instantanea de su estado en el fichero \ \', aSnapshotName ,
				'\ \Por favor tome nota de este nombre y el motivo por el que Usted ha grabado la instantanea\.',
				'Por ejemplo, grabacion del trabajo actual,\',
				'o en caso de una situacion exceptional, por favor tome nota de la relevancia de la situacion o incidencia observada.\',
				'Muchas gracias.') withCRs.
			Dialog warn: aMessage.
			Transcript show: aMessage; cr
		].

		(Delay forMilliseconds: 500) wait.
		self panelsTabsModel selectionIndex: 2.
		self panelsTabsModel selectionIndex: 1.!

techSupport
	CMAPLauncherPanel openWithSpec: #soporteWindowSpec! !

!CMAPPilot publicMethodsFor: 'application'!

applicationConfiguration

	| aManager |
	aManager := self manager.
	aManager isNil ifTrue: [ ^false].

	^aManager applicationConfiguration!

chooseApplication

	| someApplicationConfigurations someApplicationConfigurationNames aSelectedApplicationConfiguration aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^nil].

	someApplicationConfigurations := aManager candidateApplicationConfigurations.
	(someApplicationConfigurations isNil or: [ someApplicationConfigurations isEmpty]) ifTrue: [ 
		self uiWarn: 'No hay aplicaciones seleccionables'.
		^nil
	].

	someApplicationConfigurations := someApplicationConfigurations asArray.
	someApplicationConfigurationNames := someApplicationConfigurations collect: [:anApplicationConfiguration |
		anApplicationConfiguration applicationName
	].
	
	aSelectedApplicationConfiguration := Dialog 
		choose: 'Seleccione una Aplicacion a Ejecutar' 
		fromList:  someApplicationConfigurationNames
		values: someApplicationConfigurations
		lines: (someApplicationConfigurations size + 2 min: 20)
		cancel: [nil].

	aSelectedApplicationConfiguration isNil ifTrue: [ ^nil].

	Dialog warn: 'Va a Ejecutar Aplicacion CMAPware ' , aSelectedApplicationConfiguration applicationName.
self halt.
	aManager selectApplicationConfiguration: aSelectedApplicationConfiguration.

	self setupUIForApplicationConfiguration.

	^aSelectedApplicationConfiguration!

isApplicationChosen

	| aManager |
	aManager := self manager.
	aManager isNil ifTrue: [ ^false].

	^aManager isApplicationChosen!

setupUIForApplicationConfiguration

	| aMenu aCMAPMenuItem aCMAPMenuItemLabel aModelMenuItemLabel anApplicationConfiguration anApplicationName aModelKindLabel aModelMenuItem aMenuBar someMenuButtons aCMAPMenu aNewMenuItemLabel aNewMenuItem aOpenMenuItem |

	aMenu := self builder menuAt: #menuBar.
	aCMAPMenu := aMenu valueAt: self class cmapWareMenuIndex.
	"aModelMenu := aMenu valueAt: self class modelMenuIndex."

	aCMAPMenuItemLabel := nil.
	aModelMenuItemLabel := nil.
	aNewMenuItemLabel := nil.

	anApplicationConfiguration := self applicationConfiguration.
	 anApplicationConfiguration isNil ifFalse: [ 

		anApplicationName := anApplicationConfiguration applicationName.
		aCMAPMenuItemLabel := anApplicationName.

		aModelKindLabel := anApplicationConfiguration modelKindLabel.
		aModelMenuItemLabel := aModelKindLabel.
		aNewMenuItemLabel  := aModelKindLabel.
	].


	aCMAPMenuItemLabel isNil ifTrue: [ 
		aCMAPMenuItemLabel := 'CMAPware' copy.
	].
	aModelMenuItemLabel isNil ifTrue: [ 
		aModelMenuItemLabel := 'Model' copy.
	].
	aNewMenuItemLabel isNil ifTrue: [ 
		aNewMenuItemLabel := 'Model' copy.
	].

	aCMAPMenuItem := aMenu  menuItemAt: self class cmapWareMenuIndex.
	aCMAPMenuItem label: aCMAPMenuItemLabel.

	aModelMenuItem := aMenu  menuItemAt: self class modelMenuIndex.
	aModelMenuItem label: aModelMenuItemLabel.

	aMenuBar := builder keyboardProcessor menuBar.

	someMenuButtons := aMenuBar menuButtons.
	(someMenuButtons at: self class cmapWareMenuIndex ) labelString: aCMAPMenuItemLabel; invalidate.
	(someMenuButtons at: self class modelMenuIndex ) labelString: aModelMenuItemLabel; invalidate.
	aMenuBar layoutComponentsForBounds: aMenuBar bounds.
	aMenuBar invalidate.


	aNewMenuItem := aCMAPMenu  menuItemAt: self class cmapWareNewMonoMenuItemIndex.
	aNewMenuItem label: 'New ' , aNewMenuItemLabel.

	aOpenMenuItem := aCMAPMenu  menuItemAt: self class cmapWareOpenMonoMenuItemIndex.
	aOpenMenuItem label: 'Open ' , aNewMenuItemLabel.

	self updateMenuAndButtonsBar.
	self updateWindowLabel.! !

!CMAPPilot publicMethodsFor: 'aspects'!

panelsModel

	| someCanvases |

	panelsModel isNil ifFalse: [ ^panelsModel].

	someCanvases := OrderedCollection new.
	someCanvases 
		add: CMAPPilotWindowsPanel->#cmapPilotWindowsCanvasSpec;
		add: CMAPPilotMessagesPanel->#cmapPilotMessagesCanvasSpec;
		yourself.

	panelsModel := someCanvases asValue.
	^panelsModel!

panelsTabsModel

	| aTabsList |

	panelsTabsModel isNil ifFalse: [ ^panelsTabsModel].

	aTabsList := List new.
	aTabsList 
		add: 'Windows';
		add: 'Messages';
		yourself.

	panelsTabsModel := SelectionInList with: aTabsList.

	^panelsTabsModel! !

!CMAPPilot publicMethodsFor: 'browse'!

browseCurrentModel
	"CMAPInfoHolder browseCurrentModel "

	| aModel  aDefinitionsHolder aMetaInfo aBrowserClass aDomainCMGO aHomeModel anApplicationConfiguration aModelKindLabel anApplicationName aProject aHomeName aRoot aRootNameAttributeName aRootName aDefinitionsHolderFactory aManager |
	
	aManager := self manager.
	aManager isNil ifTrue: [ ^self].

	aManager isLogged ifFalse: [ ^self].

	anApplicationConfiguration := aManager applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^nil].

	aHomeName := anApplicationConfiguration homeName.
	(aHomeName isNil or: [ aHomeName isEmpty]) ifTrue: [ ^nil].

	aRootNameAttributeName := anApplicationConfiguration rootNameAttributeName.
	(aRootNameAttributeName isNil or: [ aRootNameAttributeName isEmpty]) ifTrue: [ ^nil].

	aRootName := anApplicationConfiguration rootName.
	(aRootName isNil or: [ aRootName isEmpty]) ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].

	anApplicationName := anApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aBrowserClass := anApplicationConfiguration browserClass.
	aBrowserClass isNil ifTrue: [ ^nil].


	aDefinitionsHolderFactory := aManager definitionsHolderFactory.
 	aDefinitionsHolderFactory isNil ifTrue: [ ^nil].


	aProject := aManager project.
	aProject isNil ifTrue: [
		Dialog warn: 'No hay abierta ningun ', aModelKindLabel , ' ' , anApplicationName.
		^nil].

	aDomainCMGO := aProject domainCMGO.
	aDomainCMGO isNil ifTrue: [ ^self].

	aHomeModel := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: CODEElement homesCMGODomainRelationshipName 
		detect: CODEElement homeNameCMGODomainAttributeName test: [:aHName | aHName = aHomeName].
	aHomeModel isNil ifTrue: [ ^aDomainCMGO].

	aMetaInfo := aHomeModel metaInfo.
	aMetaInfo isNil ifTrue: [ ^self].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^self].

	aRoot := aMetaInfo getObject: aHomeModel featureNamedValue: CODEElement homeRootsCMGODomainAttributeName 
		detect: aRootNameAttributeName  test: [:anModelName | anModelName = aRootName] orCreate: aRootName.
	aRoot  isNil ifTrue: [ ^aDomainCMGO].


	aDefinitionsHolder := [ | aDefHldr |
		aDefHldr := aDefinitionsHolderFactory fromModel: aModel.
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::Model')  pathSelectorNames:
			#( 'ownedElements' 'MetaInfo').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::CompositeData') pathSelectorNames:
			#('features' 'constraints' 'supertype' 'subtypes'  'typeOfFlowPorts' 'typeOfProperties' 'typeOfAttributes' 'MetaInfo' 'allFeatures').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::Protocol') pathSelectorNames:
			#('ports' 'nodes' 'abstractTransitions' 'initiator' 'responder' 'ownedElements' 'supertype' 'subtypes'  'usedByProtocolPorts' 'portsUsed' 'MetaInfo' 'allPorts').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::Interface') pathSelectorNames:
			#('ports' 'nodes' 'abstractTransitions' 'initiator' 'responder' 'ownedElements' 'supertype' 'subtypes'  'usedByProtocolPorts' 'portsUsed' 'MetaInfo' 'allPorts').
		aDefHldr redefine: (aModel typeNamed: 'mda::eca::cca::ProcessComponent') pathSelectorNames:
			#('ports' 'properties' 'uses' 'nodes' 'abstractTransitions' 'bindings' 'responder' 'ownedElements' 'supertype' 'subtypes' 'usedByComponentUsages' 'bindsToOfContextualBindings'  'portsUsed' 'MetaInfo' 'allPorts' 'allProperties').


		"aDefHldr redefine: (aModel typeNamed: 'Nucleo::Model') perspective: 'Samples' selectorNames:
			#('cif' 'razonSocial' 'nombreComercial' 'direccionOficial' 'direccionFacturaAlquiler' 
				'numeroTelecomunicacionTelefonoOficialUno' 'numeroTelecomunicacionTelefonoOficialDos'  				'numeroTelecomunicacionFaxOficial').
"
		aDefHldr
	].
	
	aBrowserClass 
		openForObject: 			aRoot 
		definitionsHolder: 		aDefinitionsHolder
		browserParameters:	
			(Dictionary new
				at:  METABrowser showCanvasLabelParameterSymbol put: true;
				at:  METABrowser numberOfEditorHoldersParameterSymbol put: 3;
				at: METABrowser windowLabelPrefixSymbol put: 'Model';
				at: METABrowser initialPathParameterSymbol put: #(#('ownedElements' ) );
					yourself)! !

!CMAPPilot publicMethodsFor: 'constants'!

saveDefaultPathFileName
	^self class saveDefaultPathFileName! !

!CMAPPilot publicMethodsFor: 'initialize-release'!

initManager
	manager := CMAPManager new.
	manager pilot: self.!

release

	| aWindowsPanel aMessagesPanel aLogged |
	aLogged := self logged.
	aLogged isNil ifFalse: [
		aLogged retractInterestsFor: self
	].
	
	aWindowsPanel := self windowsPanelApplicationModelOrNil.
	aWindowsPanel isNil ifFalse:  [ aWindowsPanel release].

	aMessagesPanel := self messagesPanelApplicationModelOrNil.
	aMessagesPanel isNil ifFalse:  [ aMessagesPanel release].

	^super release! !

!CMAPPilot publicMethodsFor: 'interface opening'!

allButOpenInterface: aSymbol 
	
	| aBuilder aWindowsApplicationModel aManager |
	aBuilder := super allButOpenInterface: aSymbol.

	(self builder componentAt: #panelsTabsID) widget tabBar accelLabelAndIconTab.

	aBuilder window
		application: self; 
		sendWindowEvents: #(#close #release).
	self logged: false.

	aManager := self manager.
	aManager isNil ifFalse: [ 
		aManager pilot: self
	].

	CMAPMETAConfiguration halt: ' was doing initialize'.

	self updateMenuAndButtonsBar.

	"self panelsTabsModel selectionIndex: 2.
	self panelsTabsModel selectionIndex: 1."
	
	aWindowsApplicationModel := self windowsPanelApplicationModel.
	aWindowsApplicationModel isNil ifFalse: [ aWindowsApplicationModel updateWindowsListView].

	^aBuilder!

buildButtonsBarWith: theBuilder
	HWUIButtonsBarBuilder new
		buildFor: theBuilder
		buttonsBar: self class buttonsBar
		helpHolder: helpHolder.!

postBuildWith: theBuilder

	| aWindowsApplicationModel |
	self buildButtonsBarWith: theBuilder.

	self logged onChangeSend: #loggedChanged to: self.

	"(builder menuAt: #menuBar) helpHolder: self helpHolder."

	aWindowsApplicationModel := self windowsPanelApplicationModelOrNil.
	aWindowsApplicationModel isNil ifFalse: [ aWindowsApplicationModel pilot: self].!

postOpenWith: aBuilder

	| aWindowsApplicationModel |

	(self builder componentAt: #panelsTabsID) widget tabBar accelLabelAndIconTab.

	aBuilder window
		application: self; 
		sendWindowEvents: #(#close #release #enter).

	CMAPMETAConfiguration initialize.

	self updateMenuAndButtonsBar.
	
	self panelsTabsModel selectionIndex: 2.
	self panelsTabsModel selectionIndex: 1.
	
	aWindowsApplicationModel := self windowsPanelApplicationModel.
	aWindowsApplicationModel isNil ifFalse: [
		aWindowsApplicationModel pilot: self.
		 aWindowsApplicationModel updateWindowsListView
	].! !

!CMAPPilot publicMethodsFor: 'preferences'!

preferredConfigurationsBrowserClass

	^self class preferredConfigurationsBrowserClass! !

!CMAPPilot publicMethodsFor: 'private'!

closeAllEditors
	| aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^self].

	aManager closeAllEditors!

removeDanglingDependents
	self class removeDanglingDependents!

systemToCleanState

	^self class systemToCleanState! !

!CMAPPilot publicMethodsFor: 'RES - additions'!

privateCopy
	self logged  retractInterestsFor: self.
	^super copy! !

!CMAPPilot publicMethodsFor: 'sub panels'!

messagesPanelApplicationModel
	
	| aPanelsTabsComponent aPanelsTabsWrapper |
	aPanelsTabsWrapper := self builder componentAt: #panelsTabsID.
	aPanelsTabsWrapper isNil ifTrue: [ ^nil].

	aPanelsTabsComponent := aPanelsTabsWrapper component component.
	aPanelsTabsComponent isNil ifTrue: [ ^nil].

	aPanelsTabsComponent cache size < 2 ifTrue: [ 
		(aPanelsTabsComponent builtPageAt: 1) isNil ifTrue: [ aPanelsTabsComponent buildPage: 1].
		(aPanelsTabsComponent builtPageAt: 2) isNil ifTrue: [ aPanelsTabsComponent buildPage: 2].
	].
	aPanelsTabsComponent cache do: [:anAssoc |  | aPanel |
		aPanel := anAssoc value source.
		(aPanel isNil not and: [ aPanel  isKindOf: CMAPPilotMessagesPanel]) ifTrue: [ ^aPanel]
	].
	^nil!

messagesPanelApplicationModelOrNil
	
	| aPanelsTabsComponent aPanelsTabsWrapper |
	aPanelsTabsWrapper := self builder componentAt: #panelsTabsID.
	aPanelsTabsWrapper isNil ifTrue: [ ^nil].

	aPanelsTabsComponent := aPanelsTabsWrapper component component.
	aPanelsTabsComponent isNil ifTrue: [ ^nil].

	aPanelsTabsComponent cache do: [:anAssoc |  | aPanel |
		aPanel := anAssoc value source.
		(aPanel isNil not and: [ aPanel  isKindOf: CMAPPilotMessagesPanel]) ifTrue: [ ^aPanel]
	].
	^nil!

windowsPanelApplicationModel
	
	| aPanelsTabsComponent aPanelsTabsWrapper |
	aPanelsTabsWrapper := self builder componentAt: #panelsTabsID.
	aPanelsTabsWrapper isNil ifTrue: [ ^nil].

	aPanelsTabsComponent := aPanelsTabsWrapper component component.
	aPanelsTabsComponent isNil ifTrue: [ ^nil].

	aPanelsTabsComponent cache size < 2 ifTrue: [ 
		(aPanelsTabsComponent builtPageAt: 1) isNil ifTrue: [ aPanelsTabsComponent buildPage: 1].
		(aPanelsTabsComponent builtPageAt: 2) isNil ifTrue: [ aPanelsTabsComponent buildPage: 2].
	].
	aPanelsTabsComponent cache do: [:anAssoc |  | aPanel |
		aPanel := anAssoc value source.
		(aPanel isNil not and: [ aPanel  isKindOf: CMAPPilotWindowsPanel]) ifTrue: [ ^aPanel]
	].
	^nil!

windowsPanelApplicationModelOrNil
	
	| aPanelsTabsComponent aPanelsTabsWrapper |
	aPanelsTabsWrapper := self builder componentAt: #panelsTabsID.
	aPanelsTabsWrapper isNil ifTrue: [ ^nil].

	aPanelsTabsComponent := aPanelsTabsWrapper component component.
	aPanelsTabsComponent isNil ifTrue: [ ^nil].

	aPanelsTabsComponent cache do: [:anAssoc |  | aPanel |
		aPanel := anAssoc value source.
		(aPanel isNil not and: [ aPanel  isKindOf: CMAPPilotWindowsPanel]) ifTrue: [ ^aPanel]
	].
	^nil! !

!CMAPPilot publicMethodsFor: 'ui'!

uiChooseFile: theFileDialogInterface

	| aFilePath |
	theFileDialogInterface isNil ifTrue: [ ^nil].

	theFileDialogInterface openFileDialog == true ifFalse: [ ^nil].

	aFilePath := theFileDialogInterface returnedFilePath.
	^aFilePath!

uiConfirm: theQuestion

	| aResult |
	(theQuestion isNil or: [ theQuestion isEmpty]) ifTrue: [ ^nil].

	aResult := Dialog confirm: theQuestion.

	^aResult!

uiConfirm: theQuestion initialAnswer: theAnswer

	| aResult |
	(theQuestion isNil or: [ theQuestion isEmpty]) ifTrue: [ ^nil].

	aResult := Dialog confirm: theQuestion initialAnswer: theAnswer.

	^aResult!

uiCursor: theCursor showWhile: theBlock

	| aValue |
	theCursor isNil ifTrue: [ ^nil].
	theBlock isNil ifTrue: [ ^nil].

	aValue := theCursor showWhile: theBlock.
	^aValue!

uiMessage: theMessage

	(theMessage isNil or: [ theMessage isEmpty]) ifTrue: [ ^nil].

	Transcript show: theMessage; cr.

	^theMessage!

uiWarn: theWarning

	(theWarning isNil or: [ theWarning isEmpty]) ifTrue: [ ^nil].

	Dialog warn: theWarning; cr.

	^theWarning!

uiWarnAndMessage: theMessage

	self uiWarn: theMessage.
	self uiMessage: theMessage! !

!CMAPPilot publicMethodsFor: 'updating'!

applicationWindowEntered
	| aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^self].

	aManager applicationWindowEntered: self!

changeRequest

	self logout ifFalse: [^false].

	^DEBUGDvpt
		ifTrue: [
			self release.
			true]
		ifFalse: [
			ObjectMemory quit.
			false]!

loggedChanged

	self updateWindowLabel.
	self updateMenuAndButtonsBar.
	self updateWindowsListView!

updateButtonsBar
	^self updateMonoButtonsBar!

updateMenu
	^self updateMonoMenu!

updateMenuAndButtonsBar
	| menu |
	self updateMenu.
	self updateButtonsBar.
	menu := self builder menuAt: #menuBar.
	HWUIButtonsBarBuilder new
		updateMenuItems: menu
		fromBar: self class buttonsBar!

updateMonoButtonsBar

	|   someButtons aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^self].

	someButtons := (self builder componentAt: #BUTTONSBAR) widget components.
	aManager isLogged
		ifFalse: [
			(someButtons at: self class newProjectButtonIndex) disable.
			(someButtons at: self class openProjectButtonIndex) disable.
			(someButtons at: self class saveProjectButtonIndex) disable]
		ifTrue: [
			(someButtons at: self class newProjectButtonIndex) enable.
			(someButtons at: self class openProjectButtonIndex) enable.
			aManager hasOpenedProject 
				ifFalse: [ (someButtons at: self class saveProjectButtonIndex) disable]
				ifTrue: [ (someButtons at: self class saveProjectButtonIndex) enable]]!

updateMonoMenu
	| aMenu  aMenuItem aCMAPMenu someMenuButtons aSistemaMenu aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^self].

	aMenu := self builder menuAt: #menuBar.
	aCMAPMenu := aMenu valueAt: self class cmapWareMenuIndex.
	aSistemaMenu := aMenu valueAt: self class sistemaMenuIndex.

aManager isApplicationChosen
	ifFalse: [ 
			aCMAPMenu 		enableIndexes: self class cmapWarePreChooseApplicationMonoMenuItemIndexes.

			aCMAPMenu 		disableIndexes: self class cmapWarePostChooseApplicationMonoMenuItemIndexes.
			aMenuItem := aCMAPMenu  menuItemAt: self class cmapWareLoginLogoutMonoMenuItemIndex.
			aMenuItem label: self class loginMonoMenuItemLabel.
			aCMAPMenu helpAt: self class cmapWareLoginLogoutMonoMenuItemIndex  put: self class loginMonoMenuItemHelp.
			aCMAPMenu valueAt: self class cmapWareLoginLogoutMonoMenuItemIndex  put: self class loginMonoMenuItemSelector.
			aCMAPMenu 		disableIndexes: self class cmapWarePostLoginMonoMenuItemIndexes.
			aCMAPMenu 		disableIndexes: self class cmapWarePostOpenMonoMenuItemIndexes.
			aSistemaMenu 			disableIndexes: self class sistemaPostLoginMonoMenuItemIndexes.
			aSistemaMenu 			disableIndexes: self class sistemaPostOpenMonoMenuItemIndexes.
			aMenu 					disableIndexes: self class postLoginMonoMenuItemIndexes.
			aMenu 					disableIndexes: self class postOpenMonoMenuItemIndexes.

			someMenuButtons := self builder keyboardProcessor menuBar menuButtons.
			self class postLoginMonoMenuItemIndexes do: [:anIndex | (someMenuButtons at: anIndex) isEnabled: false].
			self class postOpenMonoMenuItemIndexes do: [:anIndex | (someMenuButtons at: anIndex) isEnabled: false].

	]

	ifTrue: [ 
	
	aCMAPMenu 		enableIndexes: self class cmapWarePostChooseApplicationMonoMenuItemIndexes.
	aManager isLogged
		ifFalse: [
			aCMAPMenu 		enableIndexes: self class cmapWarePreChooseApplicationMonoMenuItemIndexes.
			aMenuItem := aCMAPMenu  menuItemAt: self class cmapWareLoginLogoutMonoMenuItemIndex.
			aMenuItem label: self class loginMonoMenuItemLabel.
			aCMAPMenu helpAt: self class cmapWareLoginLogoutMonoMenuItemIndex  put: self class loginMonoMenuItemHelp.
			aCMAPMenu valueAt: self class cmapWareLoginLogoutMonoMenuItemIndex  put: self class loginMonoMenuItemSelector.
			aCMAPMenu 		disableIndexes: self class cmapWarePostLoginMonoMenuItemIndexes.
			aCMAPMenu 		disableIndexes: self class cmapWarePostOpenMonoMenuItemIndexes.
			aSistemaMenu 			disableIndexes: self class sistemaPostLoginMonoMenuItemIndexes.
			aSistemaMenu 			disableIndexes: self class sistemaPostOpenMonoMenuItemIndexes.
			aMenu 					disableIndexes: self class postLoginMonoMenuItemIndexes.
			aMenu 					disableIndexes: self class postOpenMonoMenuItemIndexes.

			someMenuButtons := self builder keyboardProcessor menuBar menuButtons.
			self class postLoginMonoMenuItemIndexes do: [:anIndex | (someMenuButtons at: anIndex) isEnabled: false].
			self class postOpenMonoMenuItemIndexes do: [:anIndex | (someMenuButtons at: anIndex) isEnabled: false].

		]
		ifTrue: [

			aCMAPMenu 		disableIndexes: self class cmapWarePreChooseApplicationMonoMenuItemIndexes.
			aMenu enableIndexes: self class postLoginMonoMenuItemIndexes.

			someMenuButtons := self builder keyboardProcessor menuBar menuButtons.
			self class postLoginMonoMenuItemIndexes do: [:anIndex | (someMenuButtons at: anIndex) isEnabled: true].

			aMenuItem := aCMAPMenu  menuItemAt: self class cmapWareLoginLogoutMonoMenuItemIndex.
			aMenuItem label: self class logoutMonoMenuItemLabel.
			aCMAPMenu helpAt: self class cmapWareLoginLogoutMonoMenuItemIndex  put: self class logoutMonoMenuItemHelp.
			aCMAPMenu valueAt: self class cmapWareLoginLogoutMonoMenuItemIndex  put: self class logoutMonoMenuItemSelector.

			aCMAPMenu enableIndexes: self class cmapWarePostLoginMonoMenuItemIndexes.
			aSistemaMenu enableIndexes: self class sistemaPostLoginMonoMenuItemIndexes.

			aManager hasOpenedProject 
				ifFalse: [
					aCMAPMenu disableIndexes: self class cmapWarePostOpenMonoMenuItemIndexes.
					aSistemaMenu disableIndexes: self class sistemaPostOpenMonoMenuItemIndexes.
					aMenu disableIndexes: self class postOpenMonoMenuItemIndexes.
					self class postOpenMonoMenuItemIndexes do: [:anIndex | (someMenuButtons at: anIndex) isEnabled: false].

				]
				ifTrue: [
					aCMAPMenu enableIndexes: self class cmapWarePostOpenMonoMenuItemIndexes.
					aSistemaMenu enableIndexes: self class sistemaPostOpenMonoMenuItemIndexes.
					aMenu enableIndexes: self class postOpenMonoMenuItemIndexes.
					self class postOpenMonoMenuItemIndexes do: [:anIndex | (someMenuButtons at: anIndex) isEnabled: true].
				].



		]
	]!

updateWindowLabel

	| aUserName anApplicationName anApplicationConfiguration aManager |

	aManager := self manager.
	aManager isNil ifTrue: [ ^self].

	anApplicationName := nil.

	anApplicationConfiguration := aManager applicationConfiguration.
	anApplicationConfiguration isNil ifFalse: [
		anApplicationName := anApplicationConfiguration applicationName.
	].

	anApplicationName isNil ifTrue: [ 
		anApplicationName := 'CMAPware' copy
	].

	aUserName := aManager userName.
	self builder window label: 
		'Menu ', anApplicationName , ' ' , (( aUserName isNil or: [aUserName isEmpty]) ifTrue: [ ' (disconnected)'] ifFalse: [ ' for User ', aUserName])!

updateWindowsList

	self updateWindowsListView.!

updateWindowsListMenu

	| aMenu aWindowsListMenu |
	aMenu := self builder menuAt: #menuBar.
	aWindowsListMenu := aMenu valueAt: self class listWindowsMenuIndex.
	aWindowsListMenu buildMenu.!

updateWindowsListSelection

	| aWindowsListApplicationModel |

	aWindowsListApplicationModel := self windowsPanelApplicationModel.
	aWindowsListApplicationModel isNil ifTrue: [ ^self].

	aWindowsListApplicationModel updateWindowsListSelection!

updateWindowsListView

	| aWindowsListApplicationModel |

	alreadyUpdatedAfterOpening == true ifFalse: [ 
		(self builder window isNil not and: [ self builder window isOpen]) ifTrue: [ 
			self panelsTabsModel selectionIndex: 2.
			self panelsTabsModel selectionIndex: 1.
			alreadyUpdatedAfterOpening := true
		]
	].

	aWindowsListApplicationModel := self windowsPanelApplicationModel.
	aWindowsListApplicationModel isNil ifTrue: [ ^self].

	aWindowsListApplicationModel updateWindowsListView!

updateWindowsSelection
	
	self updateWindowsListSelection.!

windowEvent: theEvent from: theWindow

	super windowEvent: theEvent from: theWindow.

	(#(#enter ) includes: theEvent key) ifTrue: [
		self applicationWindowEntered.
		^self].! !

!CMAPPilotMessagesPanel class publicMethodsFor: 'interface specs'!

cmapPilotMessagesCanvasSpec
	"UIPainter new openOnClass: self andSelector: #cmapPilotMessagesCanvasSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware Messages' 
			#min: #(#Point 388 145 ) 
			#bounds: #(#Rectangle 275 325 817 566 ) 
			#colors: 
			#(#LookPreferences 
				#setForegroundColor: #(#ColorValue #black ) 
				#setBackgroundColor: #(#ColorValue #white ) 
				#setSelectionForegroundColor: #(#ColorValue #white ) 
				#setSelectionBackgroundColor: #(#ColorValue #black ) 
				#setBorderColor: #(#ColorValue #black ) ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#RegionSpec 
					#layout: #(#LayoutFrame 0 0 0 0 0 1 0 1 ) 
					#isOpaque: true 
					#colors: 
					#(#LookPreferences 
						#setBackgroundColor: #(#ColorValue #white ) 
						#setHiliteColor: #(#ColorValue #gray ) 
						#setShadowColor: #(#ColorValue #black ) ) 
					#lineWidth: 0 ) 
				#(#ArbitraryComponentSpec 
					#layout: #(#LayoutFrame 0 0 0 0 0 1 0 1 ) 
					#name: #messageViewID 
					#flags: 9 
					#component: #messageView ) ) ) )! !

!CMAPPilotMessagesPanel publicMethodsFor: 'aspects'!

messageView
	|  aMenu |

	messageView isNil ifFalse: [^messageView].

	messageView := TextCollectorView new model: Transcript.
	messageView controller: TextEditorController new.
	messageView controller keyboardProcessor: builder keyboardProcessor.
	aMenu := HWHelpPopUpMenu labelList: #(#(#copy "#cut" ) #(#clear ) )  values: #(#copySelection "#cut" #cancel ).

	messageView  controller menuHolder: (ValueHolder with: aMenu).

	^messageView! !

!CMAPPilotMessagesPanel publicMethodsFor: 'initialize-release'!

release
	
	messageView isNil ifFalse: [ Transcript  removeDependent: messageView].
	messageView := nil.

	^super release! !

!CMAPPilotMessagesPanel publicMethodsFor: 'interface opening'!

postOpenWith: theBuilder
	(theBuilder componentAt: #messageViewID) widget controller cancel.! !

!CMAPPilotWindowsPanel class publicMethodsFor: 'interface specs'!

cmapPilotWindowsCanvasSpec
	"UIPainter new openOnClass: self andSelector: #cmapPilotWindowsCanvasSpec"

	<resource: #canvas>
	^#(#FullSpec 
		#window: 
		#(#WindowSpec 
			#label: 'CMAPware Windows list' 
			#min: #(#Point 388 145 ) 
			#bounds: #(#Rectangle 254 333 796 574 ) 
			#colors: 
			#(#LookPreferences 
				#setForegroundColor: #(#ColorValue #black ) 
				#setBackgroundColor: #(#ColorValue #white ) 
				#setSelectionForegroundColor: #(#ColorValue #white ) 
				#setSelectionBackgroundColor: #(#ColorValue #black ) 
				#setBorderColor: #(#ColorValue #black ) ) 
			#isEventDriven: true ) 
		#component: 
		#(#SpecCollection 
			#collection: #(
				#(#SequenceViewSpec 
					#layout: #(#LayoutFrame 0 0 0 0 0 1 0 1 ) 
					#name: #windowsListID 
					#isOpaque: true 
					#model: #windowsList 
					#selectionType: #highlight ) ) ) )! !

!CMAPPilotWindowsPanel publicMethodsFor: 'accessing'!

pilot
	^pilot!

pilot: thePilot
	pilot := thePilot! !

!CMAPPilotWindowsPanel publicMethodsFor: 'aspects'!

windowsList
	"This method was generated by UIDefiner.  Any edits made here
	may be lost whenever methods are automatically defined.  The
	initialization provided below may have been preempted by an
	initialize method."

	^windowsList isNil
		ifTrue:
			[windowsList := SelectionInList new]
		ifFalse:
			[windowsList]! !

!CMAPPilotWindowsPanel publicMethodsFor: 'initialize-release'!

release
	
	windowsList isNil ifFalse: [ windowsList release].
	windowsList := nil.

	^super release! !

!CMAPPilotWindowsPanel publicMethodsFor: 'interface opening'!

postBuildWith: theBuilder

	(theBuilder componentAt: #windowsListID) widget displayStringSelector: #label.
	self windowsList selectionIndexHolder onChangeSend: #windowsListSelectionChanged to: self! !

!CMAPPilotWindowsPanel publicMethodsFor: 'updating'!

animateZoomDuration
	^CMAPMETAConfiguration current getParameter: #animateZoomDuration default: 200!

animateZoomToWindow: theWindow
	| aZoomInitRectangle aZoomEndRectangle aDuration |

	aDuration := self animateZoomDuration.
	aDuration < 1 ifTrue:  [ ^self].

	aZoomInitRectangle := Window currentWindow globalOrigin +  Window currentWindow sensor cursorPoint extent: 2@2.
	aZoomEndRectangle := theWindow displayBox.
	Screen default zoom: aZoomInitRectangle to: aZoomEndRectangle duration: 200.!

animateZoomToWindowCursor: theWindow
	| aZoomInitRectangle aZoomEndRectangle aDuration |

	aDuration := self animateZoomDuration.
	aDuration < 1 ifTrue:  [ ^self].

	aZoomInitRectangle := theWindow displayBox.
	aZoomEndRectangle := theWindow globalOrigin +  theWindow sensor cursorPoint extent: 2@2.
	Screen default zoom: aZoomInitRectangle to: aZoomEndRectangle  duration: 200.!

updateWindowsListSelection

	| aGlobalEditorsOpener someWindows aLastEditorEntered aLastWindowEntered |

	pilot isNil ifTrue: [ ^self].

	aGlobalEditorsOpener := pilot editorsOpener.
	someWindows := self windowsList list.

	aLastEditorEntered := aGlobalEditorsOpener lastEditorEntered.
	aLastEditorEntered isNil ifFalse: [ 
		aLastWindowEntered := aLastEditorEntered builder window.
		(someWindows includes: aLastWindowEntered ) ifTrue: [ 
			inTransition == true ifFalse: [ 
				[
					inTransition := true.
					self windowsList selection: aLastWindowEntered.
				] valueNowOrOnUnwindDo: [ inTransition := false]				
			]
		]
	]!

updateWindowsListView

	| aGlobalEditorsOpener someWindows aLastEditorEntered aLastWindowEntered someWindowsSorted aManager |

	pilot isNil ifTrue: [ ^self].
	aManager := pilot manager.
	aManager isNil ifTrue: [ ^self].
	
	aGlobalEditorsOpener := aManager editorsOpener.
	aGlobalEditorsOpener isNil ifTrue: [ ^self].

	someWindows := aGlobalEditorsOpener isNil
		ifTrue: [#() copy]
		ifFalse: [ aGlobalEditorsOpener allWindows asArray].

	someWindowsSorted := someWindows asSortedCollection: [:a :b | a label < b label].

	self windowsList list: someWindowsSorted.

	aLastEditorEntered := aGlobalEditorsOpener lastEditorEntered.
	aLastEditorEntered isNil ifFalse: [ 
		aLastWindowEntered := aLastEditorEntered builder window.
		(someWindows includes: aLastWindowEntered ) ifTrue: [ 
			inTransition == true ifFalse: [ 
				[
					inTransition := true.
					self windowsList selection: aLastWindowEntered.
				] valueNowOrOnUnwindDo: [ inTransition := false]				
			]
		]
	]!

windowsListSelectionChanged
	
	| aWindow |
	aWindow := self windowsList selection.
	aWindow isNil ifTrue: [ ^self].

	pilot isNil  ifTrue: [ ^nil].
	inTransition == true ifTrue: [ ^nil].

	aWindow isOpen
		ifFalse: [ Dialog warn: 'La ventana CMAPware ', aWindow label, ' ya ha sido cerrada']
		ifTrue: [
			aWindow isCollapsed ifTrue: [aWindow expand].
			self animateZoomToWindow: aWindow.
			aWindow raise.
			(aWindow bounds containsPoint: aWindow sensor cursorPoint) ifFalse: [
				aWindow sensor cursorPoint: 120@70.
			].
			self animateZoomToWindowCursor: aWindow.
		]! !

!CMAPProject class publicMethodsFor: 'actions'!

createNewProject
	self shouldNotImplement!

createNewProjectWithApplicationConfiguration: theApplicationConfiguration
	| aProject aDomainCMGO aInfoStoreClass aProjectName aMustResetCurrentInfos aDefaultProjectName aDomainFactoryMethodSelector |

	theApplicationConfiguration isNil ifTrue: [ ^nil].

	aInfoStoreClass := theApplicationConfiguration infoHolderClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].

	aDomainFactoryMethodSelector := theApplicationConfiguration domainFactoryMethodSelector.
	aDomainFactoryMethodSelector isNil ifTrue: [ ^nil].

	aMustResetCurrentInfos := theApplicationConfiguration mustResetCurrentInfos == true.
	aMustResetCurrentInfos ifTrue: [ aInfoStoreClass resetCurrentInfos].

	aDefaultProjectName := self defaultProjectNameWithApplicationConfiguration: theApplicationConfiguration.
	(aDefaultProjectName isNil or: [ aDefaultProjectName isEmpty]) ifTrue: [ ^nil].

	aDomainCMGO := aInfoStoreClass  perform: aDomainFactoryMethodSelector with: theApplicationConfiguration.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aProject := self new.
	aProject domainCMGO: aDomainCMGO.

	aProjectName := aDefaultProjectName.
	
	(aProjectName isNil or: [ aProjectName isEmpty]) ifTrue: [
		aProjectName := aDomainCMGO metaInfo getObject: aDomainCMGO 
			featureNamedValue: aDomainCMGO metaInfo class  domainNameCMGODomainAttributeName. 
	].
	aProject name: aProjectName.
	aProject applicationConfiguration: theApplicationConfiguration.

	^aProject!

openProjectMonoInFolder: theFolder
		self shouldNotImplement!

openProjectMonoInFolder: theFolder withApplicationConfiguration: theApplicationConfiguration
	| aFolder	aFileDialogInterface aFilename aFilePath aFilePathFilename aSavePostfix aInfoStoreClass aDomainCMGO aProject aFolderName aProjectName aDotIndex aModelKindLabel aDefaultProjectName aInfoStoreMethodSelector anApplicationName |

	theApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aInfoStoreClass := theApplicationConfiguration infoStoreClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].

	aInfoStoreMethodSelector := theApplicationConfiguration infoStoreMethodSelector.
	aInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	anApplicationName := theApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aSavePostfix := theApplicationConfiguration savePostfix.
	(aSavePostfix isNil or: [ aSavePostfix isEmpty])  ifTrue: [ ^nil].

	aModelKindLabel := theApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].


	aFileDialogInterface := FileDialogInterface new.
	aFileDialogInterface isNil ifTrue: [ ^nil].
	aFileDialogInterface titleBarText: 'Abrir Fichero ', aModelKindLabel , (self name isNil ifTrue: [ ' - sin nombre - '] ifFalse: [ self name]).
	aFileDialogInterface addOption: #fileMustExist.
	aFileDialogInterface fileFilter: ((OrderedCollection new: 1) 
		add:  (Association key:  'Ficheros de ' , aModelKindLabel , ' de ' , anApplicationName , '  (*', aSavePostfix, ')' value: '*' ,aSavePostfix); 
		add:  (Association key: 'Todos los ficheros (*.*)' value: '*.*'); 
		yourself).

	aFolder := theFolder.
	(aFolder isNil not and: [ aFolder isEmpty not]) ifTrue: [
		aFilename := aFolder asFilename.
		(aFilename exists and: [aFilename isDirectory]) ifTrue: [
			aFileDialogInterface initialDirectory: aFolder]].

	aFileDialogInterface openFileDialog == true ifFalse: [ ^false].

	aFilePath := aFileDialogInterface returnedFilePath.
	aFilePath isNil ifTrue: [ ^nil].
	
	aFilePathFilename := aFilePath asFilename.

	aFilePathFilename asFilename exists ifFalse: [
		Dialog warn: 'Fichero ', aFilePathFilename, ' no existe'.
		^nil].

	Transcript show: 'Cargando ', aModelKindLabel , ' desde el fichero', aFilePath , '...'; cr.


	aInfoStoreClass resetCurrentInfoStoreMethodSelector: aInfoStoreMethodSelector withApplicationConfiguration: theApplicationConfiguration.

	aDomainCMGO := aInfoStoreClass loadCurrentInfoModelFrom: aFilePath withApplicationConfiguration: theApplicationConfiguration.

	(aDomainCMGO isKindOf: CMGenericObject) ifFalse: [
		Transcript show: 'No ha sido posible cargar un  ', aModelKindLabel , '  ' , anApplicationName , ' desde el fichero ', aFilePath ; cr.
		^nil].
		
	aProject := self new.
	aProject domainCMGO: aDomainCMGO.

	aDefaultProjectName := self defaultProjectNameWithApplicationConfiguration: theApplicationConfiguration.

	aFolderName := aFilePathFilename head.
	aProjectName := aFilePathFilename tail.
	aDotIndex := aProjectName indexOf: $..
	aDotIndex  > 0 ifTrue: [ 
		aProjectName := (aDotIndex = 1 or: [aProjectName size < 2])
			ifTrue: [ 
				((aDefaultProjectName isNil or: [ aDefaultProjectName isEmpty]) ifTrue: [ 'project'] ifFalse: [ aDefaultProjectName ])]
			ifFalse: [ aProjectName copyFrom: 1 to: aDotIndex - 1]
	].

	aProject folder: aFolderName.
	aProject name: aProjectName.

	^aProject!

openProjectMonoWithApplicationConfiguration: theApplicationConfiguration
	^self openProjectMonoInFolder: nil withApplicationConfiguration: theApplicationConfiguration! !

!CMAPProject class publicMethodsFor: 'defaults'!

backupPostfix
	self shouldNotImplement!

backupPostfixWithApplicationConfiguration: theApplicationConfiguration

	| aBackupPostfix |
	theApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aBackupPostfix := theApplicationConfiguration backupPostfix.
	(aBackupPostfix isNil or: [ aBackupPostfix isEmpty]) ifTrue: [ ^nil].
	^aBackupPostfix!

defaultFolderName
	^Filename findDefaultDirectory asString!

defaultProjectName
	self shouldNotImplement!

defaultProjectNameWithApplicationConfiguration: theApplicationConfiguration

	| aDefaultProjectName |
	theApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aDefaultProjectName := theApplicationConfiguration defaultProjectName.
	(aDefaultProjectName isNil or: [ aDefaultProjectName isEmpty]) ifTrue: [ ^nil].

	^aDefaultProjectName!

savePostfix
	self shouldNotImplement!

savePostfixWithApplicationConfiguration: theApplicationConfiguration

	| aSavePostfix |
	theApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aSavePostfix := theApplicationConfiguration savePostfix.
	(aSavePostfix isNil or: [ aSavePostfix isEmpty]) ifTrue: [ ^nil].
	^aSavePostfix! !

!CMAPProject class publicMethodsFor: 'info store ref'!

infoStoreClass
	self shouldNotImplement! !

!CMAPProject publicMethodsFor: 'accessing'!

applicationConfiguration
	^applicationConfiguration!

domainCMGO

	^domainCMGO!

domainCMGO: theDomainCMGO

	domainCMGO := theDomainCMGO.
	self changed: #domainCMGO!

folder

	folder isNil ifTrue: [ folder := self class defaultFolderName].
	^folder!

folder: theFolder

	folder := theFolder.
	self changed: #folder!

name
	name isNil ifTrue: [ name := self class defaultProjectNameWithApplicationConfiguration: self applicationConfiguration].
	^name!

name: theName
	name := theName.
	self changed: #name!

saveFileName
	^self name , self class savePostfixWithApplicationConfiguration: self applicationConfiguration!

saveFilePath
	^(self folder asFilename construct: self saveFileName) asString! !

!CMAPProject publicMethodsFor: 'actions'!

close

	self closeUnconditionally!

closeInApplicationConfiguration
	| aDomainCMGO aInfoStoreClass aInfoStoreMethodSelector aMustResetCurrentInfosOnClose anApplicationConfiguration |

	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aDomainCMGO := self domainCMGO.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aInfoStoreClass := anApplicationConfiguration infoStoreClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].

	aInfoStoreMethodSelector := anApplicationConfiguration infoStoreMethodSelector.
	aInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	aMustResetCurrentInfosOnClose := anApplicationConfiguration mustResetCurrentInfosOnClose == true.
	aMustResetCurrentInfosOnClose ifTrue: [ aInfoStoreClass resetCurrentInfos].

	(aInfoStoreClass hasCurrentInfoStoreMethodSelector: aInfoStoreMethodSelector) ifFalse: [ ^nil].

	aInfoStoreClass resetCurrentInfoStoreMetodSelector: aInfoStoreMethodSelector withApplicationConfiguration: anApplicationConfiguration!

closeUnconditionally

	self closeInApplicationConfiguration.

	self release!

save
	| aFilePath |
	aFilePath := self saveFilePath.
	(aFilePath isNil or: [ aFilePath isEmpty]) ifTrue: [ ^nil].

	self saveInApplicationConfigurationAs: aFilePath!

saveAs: theFilePath

	self saveInApplicationConfigurationAs: theFilePath!

saveInApplicationConfigurationAs: theFilePath 
	| aDomainCMGO aInfoStoreClass aInfoStoreMethodSelector aMustResetCurrentInfosOnClose anApplicationConfiguration |

	(theFilePath isNil or: [ theFilePath isEmpty]) ifTrue: [ ^nil].

	anApplicationConfiguration := self applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].
	
	aDomainCMGO := self domainCMGO.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aInfoStoreClass := anApplicationConfiguration infoStoreClass.
	aInfoStoreClass isNil ifTrue: [ ^nil].

	aInfoStoreMethodSelector := anApplicationConfiguration infoStoreMethodSelector.
	aInfoStoreMethodSelector isNil ifTrue: [ ^nil].

	aMustResetCurrentInfosOnClose := anApplicationConfiguration mustResetCurrentInfosOnClose == true.
	aMustResetCurrentInfosOnClose ifTrue: [ aInfoStoreClass resetCurrentInfos].

	(aInfoStoreClass hasCurrentInfoStoreMethodSelector: aInfoStoreMethodSelector) ifFalse: [ ^nil].


	^aInfoStoreClass saveCurrentInfoStoreMethodSelector: aInfoStoreMethodSelector as: theFilePath
		withApplicationConfiguration: anApplicationConfiguration! !

!CMAPProject publicMethodsFor: 'initialize-release'!

applicationConfiguration: theApplicationConfiguration
	applicationConfiguration := theApplicationConfiguration!

release

	name := nil.
	folder := nil.
	domainCMGO := nil.! !

!CMAPProject publicMethodsFor: 'svce'!

backupExistingSaveFile

	| aProjectName aBackupProjectPostfix aFileSystemMatchString aNameTermination aMatchTemplate someExistingFileNames anIndex aMaxIndex aNumber aNewIndex aNewFileName aProjectFilePath aProjectFolder aMessage anApplicationConfiguration aModelKindLabel aService anApplicationSession |

	aService := self service.
	aService isNil ifTrue: [ ^nil].
	
	anApplicationSession := aService applicationSession.
	anApplicationSession isNil ifTrue: [ ^nil].

	anApplicationConfiguration := anApplicationSession applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].

	aProjectFilePath := self saveFilePath.
	(aProjectFilePath isNil or: [ aProjectFilePath isEmpty]) ifTrue: [ ^nil].
	aProjectFilePath asFilename exists ifFalse: [ ^nil].	

	aProjectFolder := self folder.
	aProjectName := self name.
	(aProjectName isNil or: [ aProjectName isEmpty]) ifTrue: [ ^nil].

	aBackupProjectPostfix :=  self  backupPostfix.
	aFileSystemMatchString := '*'.
	aNameTermination := self savePostfix.

	aMatchTemplate := aProjectFolder , (String with: Filename separator) , 
		aProjectName, aBackupProjectPostfix , aFileSystemMatchString, aNameTermination.

	someExistingFileNames := Filename filesMatching: aMatchTemplate.
	anIndex := someExistingFileNames isEmpty
		ifTrue: [ 0]
		ifFalse: [ 
			aMaxIndex := 0.
			someExistingFileNames do: [:aFileName |
				aNumber := Object errorSignal handle: [:anEx | anEx returnWith: 0]
					do: [ Number readFrom: (aFileName copyFrom: aProjectName size + 1 to: aFileName size) readStream].
				aNumber > aMaxIndex ifTrue: [ aMaxIndex := aNumber].
			].
			aMaxIndex
		].

	aNewIndex := anIndex.
	[
		aNewIndex := aNewIndex + 1.
		aNewFileName := aProjectFolder , (String with: Filename separator) , 
			aProjectName, aBackupProjectPostfix , aNewIndex printString,  aNameTermination.
		aNewFileName asFilename exists
	] whileTrue.

		
	aProjectFilePath asFilename renameTo: aNewFileName.

	aMessage := 'Una Copia de Seguridad\del archivo de ',  aModelKindLabel , '\' withCRs, aProjectName, '\ha sido guardada en el directorio\' withCRs,
		aProjectFolder, '\en el fichero\' withCRs,
		aProjectName, aBackupProjectPostfix , aNewIndex printString,  aNameTermination.
	anApplicationSession uiWarnAndMessage: aMessage.!

closeAllowed
	^self hasPendingChanges not!

hasPendingChanges
	^false!

root

	| aModel  aMetaInfo aDomainCMGO aHomeModel anApplicationConfiguration aHomeName aRoot aRootNameAttributeName aRootName aDefinitionsHolderFactory aService anApplicationSession |
	
	aService := self service.
	aService isNil ifTrue: [ ^nil].
	
	anApplicationSession := aService applicationSession.
	anApplicationSession isNil ifTrue: [ ^nil].

	anApplicationConfiguration := anApplicationSession applicationConfiguration.
	anApplicationConfiguration isNil ifTrue: [ ^nil].

	aHomeName := anApplicationConfiguration homeName.
	(aHomeName isNil or: [ aHomeName isEmpty]) ifTrue: [ ^nil].

	aRootNameAttributeName := anApplicationConfiguration rootNameAttributeName.
	(aRootNameAttributeName isNil or: [ aRootNameAttributeName isEmpty]) ifTrue: [ ^nil].

	aRootName := anApplicationConfiguration rootName.
	(aRootName isNil or: [ aRootName isEmpty]) ifTrue: [ ^nil].

	aDefinitionsHolderFactory := self definitionsHolderFactory.
 	aDefinitionsHolderFactory isNil ifTrue: [ ^nil].

	aDomainCMGO := self domainCMGO.
	aDomainCMGO isNil ifTrue: [ ^nil].

	aHomeModel := aDomainCMGO metaInfo getObject: aDomainCMGO featureNamedValue: CODEElement homesCMGODomainRelationshipName 
		detect: CODEElement homeNameCMGODomainAttributeName test: [:aHName | aHName = aHomeName].
	aHomeModel isNil ifTrue: [ ^nil].

	aMetaInfo := aHomeModel metaInfo.
	aMetaInfo isNil ifTrue: [ ^nil].

	aModel := aMetaInfo model.
	aModel isNil ifTrue: [ ^nil].

	aRoot := aMetaInfo getObject: aHomeModel featureNamedValue: CODEElement homeRootsCMGODomainAttributeName 
		detect: aRootNameAttributeName  test: [:anModelName | anModelName = aRootName] orCreate: aRootName.
	aRoot  isNil ifTrue: [ ^nil].

	^aRoot! !

!CMAPService class publicMethodsFor: 'instance creation'!

newForApplicationSession: theApplicationSession
	| aService |
	theApplicationSession isNil ifTrue: [ ^nil].
	aService := self new.
	aService forApplicationSession: theApplicationSession.
	^aService! !

!CMAPService publicMethodsFor: 'configurations'!

applicationConfiguration

	| aApplicationSession |
	aApplicationSession := self applicationSession.
	aApplicationSession isNil ifTrue: [ ^nil].

	^aApplicationSession applicationConfiguration! !

!CMAPService publicMethodsFor: 'initialize-release'!

forApplicationSession: theApplicationSession
	applicationSession := theApplicationSession! !

!CMAPService publicMethodsFor: 'sessions'!

applicationSession
	^applicationSession! !

!CMAPService publicMethodsFor: 'svce'!

close

	self closeAllowed ifFalse: [ ^false].

	self closeUnconditionally.
	^true!

closeAllowed

	^self closeProjectAllowed!

closeProject

	| anApplicationSession anApplicationConfiguration anApplicationName aModelKindLabel aProject aPrjName |
	self closeProjectAllowed ifFalse: [ ^false].

	anApplicationSession := self applicationSession.
	 anApplicationSession isNil ifTrue: [ ^nil].

	anApplicationConfiguration := anApplicationSession applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^false].

	anApplicationName := anApplicationConfiguration applicationName.
	(anApplicationName isNil or: [ anApplicationName isEmpty])  ifTrue: [ ^false].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^false].

	aProject := self project.
	aProject isNil ifTrue: [ ^true].

	aPrjName := aProject name.

	(anApplicationSession uiConfirm: 'Realmente desea descartar ' , aModelKindLabel , ' actual en ', anApplicationName , '\' withCRs, 
		aPrjName, ' ?' initialAnswer: false) ifFalse: [^false].

	self closeProjectUnconditionally.
	^true!

closeProjectAllowed

	| aProject |

	aProject := self project.
	aProject isNil ifTrue: [ ^true].

	^aProject closeAllowed!

closeProjectUnconditionally
	| aProject  anApplicationConfiguration aModelKindLabel anApplicationName aPrjName anApplicationSession |

	anApplicationSession := self applicationSession.
	 anApplicationSession isNil ifTrue: [ ^nil].

	anApplicationConfiguration := anApplicationSession applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^false].

	anApplicationName := anApplicationConfiguration applicationName.
	(anApplicationName isNil or: [ anApplicationName isEmpty])  ifTrue: [ ^false].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^false].

	anApplicationSession aboutToCloseProject.

	aProject := self project.
	aProject isNil ifTrue: [ ^true].

	aPrjName := aProject name.

	aProject closeUnconditionally.

	self project: nil.

	anApplicationSession uiMessage: anApplicationName , ' ' , aModelKindLabel , '  named ', aPrjName, ' discarded.'; cr.

	anApplicationSession optimizeResources.

	^true!

closeUnconditionally

	| aApplicationSession |

	aApplicationSession := self applicationSession.
	aApplicationSession isNil ifFalse: [ 
		aApplicationSession aboutToCloseService
	].

	self closeProjectUnconditionally.


	self release.!

hasOpenedProject
	^project isNil not!

openProject
	| aProject aMessage anApplicationConfiguration aProjectClass aModelKindLabel anApplicationName anApplicationSession |

	anApplicationSession := self applicationSession.
	 anApplicationSession isNil ifTrue: [ ^nil].

	anApplicationConfiguration := anApplicationSession applicationConfiguration.
	 anApplicationConfiguration isNil ifTrue: [ ^nil].

	anApplicationName := anApplicationConfiguration applicationName.
	anApplicationName isNil ifTrue: [ ^nil].

	aProjectClass := anApplicationConfiguration projectClass.
	aProjectClass isNil ifTrue: [ ^nil].

	aModelKindLabel := anApplicationConfiguration modelKindLabel.
	(aModelKindLabel isNil or: [ aModelKindLabel isEmpty])  ifTrue: [ ^nil].

	aProjectClass := anApplicationConfiguration projectClass.
	aProjectClass isNil ifTrue: [ ^nil].

	self closeProject ifFalse: [^nil].
	
	anApplicationSession uiCursor: Cursor wait showWhile: [ 
		anApplicationSession uiMessage:  'Reading ', aModelKindLabel , '  ...';cr; show: '...Please wait a few seconds';cr.

		aProject := aProjectClass openProjectInFolder: self defaultPath withManager: self.
		aProject == false ifTrue: [
			aMessage :=  'User cancelled read operation'.
			anApplicationSession uiWarnAndMessage: aMessage; cr.
			^nil
		].
		aProject isNil 
			ifTrue: [ 
				aMessage :=  ('Error while reading model.\', 
					'Please, take a Snapshort (menu option in ' , anApplicationName , '>>Snapshot)\',
					'and contact  ' , anApplicationName , ' service.',
					'Thank you.') withCRs.
				anApplicationSession uiWarnAndMessage: aMessage.
				^nil
			]
			ifFalse: [ 
				aMessage :=  (aModelKindLabel , '  read completed.\',
					'You may continue working.\',
					'Please note that the  ', aModelKindLabel , '  menu has been enabled.') withCRs.
				anApplicationSession uiWarnAndMessage: aMessage.
			].
	].


	self project: aProject.

	anApplicationSession uiMessage: anApplicationName , '   ', aModelKindLabel , '  named ', aProject name ,  ' loaded. ';
		show: Date today printString; show: Time now printString;cr.

	anApplicationSession projectHasBeenOpened.

	^aProject! !

!CMAPTranslationHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentTranslation browsePath"
	"self  resetCurrentTranslations"
	"(self  currentTranslationStoreMethodSelector: self defaultCurrentTranslationSelector) browsePath"
	"self  resetTranslationSelectorsToIgnore"
	"self  translationSelectorsToIgnore"
	"self  translationSelectorsToIgnoreAdd: self defaultCurrentTranslationSelector"! !

!CMAPTranslationHolder class publicMethodsFor: 'default'!

defaultCurrentTranslationSelector
	"CMAPTranslationHolder  defaultCurrentTranslationSelector "

	^#omgmofSimpleNoTranslationStore! !

!CMAPTranslationHolder class publicMethodsFor: 'translations persistence'!

cmapNoTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMAPTranslationHolder cmapNoTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'OmgEdocSimple_ApplicationTranslation'
	CMAPTranslationHolder cmapNoTranslationStore
	nil
	nil
   )!

cmapTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMAPTranslationHolder cmapTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'OmgEdocSimple_ApplicationTranslation'
	CMAPTranslationHolder cmapTranslationStore
	nil
	(groups
	  ( group 'Domain_Module'
		nil
		(items
		  ( item 'Domain_Module'
			'Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements_Module'
		nil
		(items
		  ( item 'DomainRootElements_Module'
			'Elementos Raiz del Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'domainRoots_Module'
		nil
		(items
		  ( item 'domainRoots_Module'
			'Modulo de Dominio (Modelo)'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'domainRoots_Model'
		nil
		(items
		  ( item 'domainRoots_Model'
			'Modelo de Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements::Domain_Type'
		nil
		(items
		  ( item 'Domain_Type'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homes_Relationship'
			'Origenes'
			nil nil
			nil
		   )

		  ( item 'homes_Relationship_NameOne'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domainModelCMGO_Attribute'
			'ModeloDominio'
			nil nil
			nil
		   )

		  ( item 'domainModelCMGO_Attribute_NameOne'
			'domainModelCMGO'
			nil nil
			nil
		   )

		  ( item 'homesCMGO_Relationship'
			'Origenes'
			nil nil
			nil
		   )

		  ( item 'homesCMGO_Relationship_NameOne'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domainNameCMGO_Attribute'
			'NombreDominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'DomainRootElements::Home_Type'
		nil
		(items
		  ( item 'Home_Type'
			'Origen'
			nil nil
			nil
		   )

		  ( item 'domain_Relationship'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homeIDCounterCMGO_Attribute'
			'ContadorIdentificadores'
			nil nil
			nil
		   )

		  ( item 'homeIDCounterCMGO_Attribute_NameOne'
			'homeIDCounterCMGO'
			nil nil
			nil
		   )

		  ( item 'homeRootsCMGO_Attribute'
			'Raices'
			nil nil
			nil
		   )

		  ( item 'homeRootsCMGO_Attribute_NameOne'
			'Raiz'
			nil nil
			nil
		   )

		  ( item 'metaInfoCMGO_Attribute'
			'MetaInfo'
			nil nil
			nil
		   )

		  ( item 'metaInfoCMGO_Attribute_NameOne'
			'metaInfoCMGO'
			nil nil
			nil
		   )

		  ( item 'homedElementsTypeMetaInfoCMGO_Attribute'
			'TipoRaizMetaInfo'
			nil nil
			nil
		   )

		  ( item 'homedElementsTypeMetaInfoCMGO_Attribute_NameOne'
			'homedElementsTypeMetaInfoCMGO'
			nil nil
			nil
		   )

		  ( item 'domainCMGO_Relationship'
			'Dominio'
			nil nil
			nil
		   )

		  ( item 'homeNameCMGO_Attribute'
			'NombreOrigen'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Integer_Type'
		nil
		(items
		  ( item 'Integer_Type'
			'Entero'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEElement_Type'
		nil
		(items
		  ( item 'CODEElement_Type'
			'MetaInfo'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEType_Type'
		nil
		(items
		  ( item 'CODEType_Type'
			'Tipo%1'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CMGO_Type'
		nil
		(items
		  ( item 'CMGO_Type'
			'ObjetoGenerico'
			nil nil
			nil
		   )

		  ( item 'objectDomainCMGO_Attribute'
			'Dominio'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::CODEModel_Type'
		nil
		(items
		  ( item 'CODEModel_Type'
			'Modelo%1'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes_Module'
		nil
		(items
		  ( item 'PrimitiveTypes_Module'
			'Tipos Primitivos'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::String_Type'
		nil
		(items
		  ( item 'String_Type'
			'Letras'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Aux_Module'
		nil
		(items
		  ( item 'Aux_Module'
			'AuxiliarUsoInterno'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo_Module'
		nil
		(items
		  ( item 'Nucleo_Module'
			'Nucleo'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Archivo_Type'
		nil
		(items
		  ( item 'Archivo_Type'
			'Archivo'
			nil nil
			nil
		   )

		  ( item 'historias_Relationship'
			'historias'
			nil nil
			nil
		   )

		  ( item 'historias_Relationship_NameOne'
			'Historia'
			nil nil
			nil
		   )

		  ( item 'personaModels_Relationship'
			'personasModel'
			nil nil
			nil
		   )

		  ( item 'personaModels_Relationship_NameOne'
			'personaModel'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Historia_Type'
		nil
		(items
		  ( item 'Historia_Type'
			'Historia'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Nucleo::Paciente_Type'
		nil
		(items
		  ( item 'Paciente_Type'
			'Paciente'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Persona_Type' 'Nucleo::Persona_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		  ( item 'direccions_Relationship'
			'direcciones'
			nil nil
			nil
		   )

		  ( item 'direccions_Relationship_NameOne'
			'direccion'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::DNINIF_Type'
		nil
		(items
		  ( item 'DNINIF_Type'
			'DNINIF'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Date_Type'
		nil
		(items
		  ( item 'Date_Type'
			'Fecha'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Time_Type'
		nil
		(items
		  ( item 'Time_Type'
			'Hora'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Number_Type'
		nil
		(items
		  ( item 'Number_Type'
			'Numero'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::CodigoHistoria_Type'
		nil
		(items
		  ( item 'CodigoHistoria_Type'
			'CodigoHistoria (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'DNINIF_Type' 'TiposBasicos::DNINIF_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Persona_Type'
		nil
		(items
		  ( item 'Persona_Type'
			'Persona'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::NumeroTelefonico_Type'
		nil
		(items
		  ( item 'NumeroTelefonico_Type'
			'NumeroTelefonico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::Telefono_Type'
		nil
		(items
		  ( item 'Telefono_Type'
			'TEL (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'NumeroTelefonico_Type' 'TiposBasicos::NumeroTelefonico_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::Fax_Type'
		nil
		(items
		  ( item 'Fax_Type'
			'FAX (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'NumeroTelefonico_Type' 'TiposBasicos::NumeroTelefonico_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::URL_Type'
		nil
		(items
		  ( item 'URL_Type'
			'URL'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::PaginaWeb_Type'
		nil
		(items
		  ( item 'PaginaWeb_Type'
			'PaginaWeb (%1)'
			nil nil
			(usedItems
			   ( refToItemTranslation 'URL_Type' 'TiposBasicos::URL_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCreacion_Type'
		nil
		(items
		  ( item 'FechaCreacion_Type'
			'%1 Creacion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::PersonaModel_Type'
		nil
		(items
		  ( item 'PersonaModel_Type'
			'PersonaModel'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Persona_Type' 'Nucleo::Persona_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::Exploracion_Type'
		nil
		(items
		  ( item 'Exploracion_Type'
			'Exploracion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::Concepto_Type'
		nil
		(items
		  ( item 'Concepto_Type'
			'Concepto'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'Conceptos::Diagnostico_Type'
		nil
		(items
		  ( item 'Diagnostico_Type'
			'Diagnostico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::Tratamiento_Type'
		nil
		(items
		  ( item 'Tratamiento_Type'
			'Tratamiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::ResultadosAnalisis_Type'
		nil
		(items
		  ( item 'ResultadosAnalisis_Type'
			'ResultadosAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Conceptos::SolicitudAnalisis_Type'
		nil
		(items
		  ( item 'SolicitudAnalisis_Type'
			'SolicitudAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Concepto_Type' 'Nucleo::Concepto_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'Nucleo::ActoClinico_Type'
		nil
		(items
		  ( item 'ActoClinico_Type'
			'ActoClinico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'ActosClinicos::Diagnostico_Type'
		nil
		(items
		  ( item 'Diagnostico_Type'
			'Diagnostico'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::Exploracion_Type'
		nil
		(items
		  ( item 'Exploracion_Type'
			'Exploracion'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::ResultadosAnalisis_Type'
		nil
		(items
		  ( item 'ResultadosAnalisis_Type'
			'ResultadosAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::SolicitudAnalisis_Type'
		nil
		(items
		  ( item 'SolicitudAnalisis_Type'
			'SolicitudAnalisis'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'ActosClinicos::Tratamiento_Type'
		nil
		(items
		  ( item 'Tratamiento_Type'
			'Tratamiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'ActoClinico_Type' 'Nucleo::ActoClinico_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaFactura_Type'
		nil
		(items
		  ( item 'FechaFactura_Type'
			'%1 Factura'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaInicioValidez_Type'
		nil
		(items
		  ( item 'FechaInicioValidez_Type'
			'FechaInicioValidez'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaFinValidez_Type'
		nil
		(items
		  ( item 'FechaFinValidez_Type'
			'FechaFinValidez'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaNacimiento_Type'
		nil
		(items
		  ( item 'FechaNacimiento_Type'
			'FechaNacimiento'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'SpanishDate_Type' 'PrimitiveTypes::SpanishDate_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCambioEstado_Type'
		nil
		(items
		  ( item 'FechaCambioEstado_Type'
			'FechaCambioEstado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::MotivoRecomendacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'MotivoRecomendacionCambioEstadoAdministrativo_Type'
			'MotivoRecomendacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::RespuestaAutorizacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'RespuestaAutorizacionCambioEstadoAdministrativo_Type'
			'RespuestaAutorizacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Boolean_Type'
		nil
		(items
		  ( item 'Boolean_Type'
			'Logico'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaAtestado_Type'
		nil
		(items
		  ( item 'FechaAtestado_Type'
			'FechaAtestado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::HoraAtestado_Type'
		nil
		(items
		  ( item 'HoraAtestado_Type'
			'HoraAtestado'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Time_Type' 'PrimitiveTypes::Time_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FirmaDigital_Type'
		nil
		(items
		  ( item 'FirmaDigital_Type'
			'FirmaDigital'
			nil nil
			(usedItems
			   ( refToItemTranslation 'String_Type' 'PrimitiveTypes::String_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioHistoria_Type'
		nil
		(items
		  ( item 'ComentarioHistoria_Type'
			'ComentarioHistoria'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'PrimitiveTypes::Text_Type'
		nil
		(items
		  ( item 'Text_Type'
			'Texto'
			nil nil
			nil
		   )

		 )
	   )

	  ( group 'TiposBasicos::ComentarioNotificacionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'ComentarioNotificacionCambioEstadoAdministrativo_Type'
			'ComentarioNotificacionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Text_Type' 'PrimitiveTypes::Text_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	  ( group 'TiposBasicos::FechaCreacionInteraccionCambioEstadoAdministrativo_Type'
		nil
		(items
		  ( item 'FechaCreacionInteraccionCambioEstadoAdministrativo_Type'
			'FechaCreacionInteraccionCambioEstadoAdministrativo'
			nil nil
			(usedItems
			   ( refToItemTranslation 'Date_Type' 'PrimitiveTypes::Date_Type' 'OmgEdocSimple_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )



	 )
   )!

cmapUITranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMAPTranslationHolder cmapUITranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'OmgEdocSimpleUI_ApplicationTranslation'
	CMAPTranslationHolder cmapUITranslationStore
	(superApplicationTranslation
	  ( refToApplicationTranslationStore 'OmgEdocSimple_ApplicationTranslation' cmapTranslationStore CMAPTranslationHolder )

	 )
	nil
   )!

m3SimpleNoTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMAPTranslationHolder m3SimpleNoTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'M3Simple_ApplicationTranslation'
	CMAPTranslationHolder m3SimpleNoTranslationStore
	nil
	(groups
	  ( group 'com::yourml::elec::Circunscripcion_Type'
		nil
		(items
		  ( item 'Circunscripcion_Type'
			'Circunscripci	n'
			nil nil
			nil
		   )

		 )
	   )

	 )
   )!

omgmofSimpleNoTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMAPTranslationHolder cmapNoTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'OmgEdocSimple_ApplicationTranslation'
	CMAPTranslationHolder cmapNoTranslationStore
	nil
	nil
   )! !

!CMAPUserConfiguration class publicMethodsFor: 'class initialization'!

initialize
	"CMAPApplicationConfiguration initialize"! !

!CMAPUserConfiguration class publicMethodsFor: 'current'!

copyCurrent
		self shouldNotImplement!

current
	self shouldNotImplement!

current: theConfiguration
	self shouldNotImplement! !

!CMAPUserConfiguration class publicMethodsFor: 'examples'!

exampleGenericUserConfiguration01

	| aConfiguration someParameters aCopy |

	aConfiguration := self new.

	aConfiguration name: self configurationName.
	aConfiguration description: self configurationDescription.

	someParameters := self configurationParameters.
	someParameters do: [:aParameter | aConfiguration parametersAdd: aParameter].

	aConfiguration recalculateParameters.

	aCopy := aConfiguration copyConfiguration.

	aCopy set: self userNameParameterName value: 'ACV' copy.
	aCopy set: self applicationConfigurationsCollectionParameterName value: CMAPApplicationConfigurationsCollection exampleGenericApplicationConfigurationsCollection01.

	aCopy recalculateParameters.

	^aCopy!

exampleUserConfiguration01

	| aConfiguration someParameters aCopy |

	aConfiguration := self new.

	aConfiguration name: self configurationName.
	aConfiguration description: self configurationDescription.

	someParameters := self configurationParameters.
	someParameters do: [:aParameter | aConfiguration parametersAdd: aParameter].

	aConfiguration recalculateParameters.

	aCopy := aConfiguration copyConfiguration.

	aCopy set: self userNameParameterName value: 'ACV' copy.
	aCopy set: self applicationConfigurationsCollectionParameterName value: CMAPApplicationConfigurationsCollection exampleApplicationConfigurationsCollection01.

	aCopy recalculateParameters.

	^aCopy! !

!CMAPUserConfiguration class publicMethodsFor: 'instance creation'!

initialConfiguration
	self shouldNotImplement!

installCurrent
	self shouldNotImplement! !

!CMAPUserConfiguration class publicMethodsFor: 'ref:configuration'!

configurationDescription
 
	^('Perfil describiendo los derechos y preferencias preestablecidas para un Usuario.\', 
		'El Administrador establece Perfiles de  Usuario permitiendo el acceso a aplicaciones seleccionadas') copy withCRs!

configurationName
	^'Perfil de Usuario' copy!

configurationParameters
 
	^(OrderedCollection new: 32)
		add: self userNameParameter;
		add: self roleNameParameter;
		add: self applicationConfigurationsCollectionParameter;
		yourself! !

!CMAPUserConfiguration class publicMethodsFor: 'ref:parameters'!

applicationConfigurationsCollectionParameter
	^self preferredParameterConfigurationsCollectionClass
			name: self applicationConfigurationsCollectionParameterName
			label: 'Application Configurations Collection'
			value: self applicationConfigurationsCollectionParameterValue
			isEditable: true
			description: ('Application Configurations Collection') withCRs
			defaultValue: self applicationConfigurationsCollectionParameterValue
			verificationBlock: nil
			derivationBlock: nil!

roleNameParameter
	^self preferredParameterStringClass
			name: self roleNameParameterName
			label: 'Role name'
			value: self roleNameParameterValue
			isEditable: true
			description: ('Role name') withCRs
			defaultValue: self roleNameParameterValue
			verificationBlock: nil
			derivationBlock: nil!

userNameParameter
	^self preferredParameterStringClass
			name: self userNameParameterName
			label: 'User name'
			value: self userNameParameterValue
			isEditable: true
			description: ('User name') withCRs
			defaultValue: self userNameParameterValue
			verificationBlock: nil
			derivationBlock: nil! !

!CMAPUserConfiguration class publicMethodsFor: 'ref:parametersvalues'!

applicationConfigurationsCollectionParameterName
	^#applicationConfigurationsCollection!

applicationConfigurationsCollectionParameterValue
	^CMAPApplicationConfigurationsCollection new!

roleNameParameterName
	^#roleName!

roleNameParameterValue
	^'Role' copy!

userNameParameterName
	^#userName!

userNameParameterValue
	^'User' copy! !

!CMAPUserConfiguration publicMethodsFor: 'custom parameter access'!

roleName
	^self getParameter: self class roleNameParameterName!

userName
	^self getParameter: self class userNameParameterName! !

!CMAPUserConfigurationsCollection class publicMethodsFor: 'class initialization'!

initialize
	"CMAPUserConfigurationsCollection initialize"! !

!CMAPUserConfigurationsCollection class publicMethodsFor: 'examples'!

exampleGenericUserConfigurationsCollection01

	| aNewUserConfigurationsCollection |
	aNewUserConfigurationsCollection := self new.
	aNewUserConfigurationsCollection 
		addConfiguration:  CMAPUserConfiguration exampleGenericUserConfiguration01.
	^aNewUserConfigurationsCollection!

exampleSpecificUserConfigurationsCollection01

	| aNewUserConfigurationsCollection aMoreGeneralUserConfigurationsCollection |
	aNewUserConfigurationsCollection := self new.
	aMoreGeneralUserConfigurationsCollection := self exampleGenericUserConfigurationsCollection01.
	aNewUserConfigurationsCollection moreGeneralConfigurationsCollectionAdd: aMoreGeneralUserConfigurationsCollection.
	^aNewUserConfigurationsCollection!

exampleUserConfigurationsCollection01

	| aNewUserConfigurationsCollection |
	aNewUserConfigurationsCollection := self new.
	aNewUserConfigurationsCollection 
		addConfiguration:  CMAPUserConfiguration exampleUserConfiguration01.
	^aNewUserConfigurationsCollection! !

!CMAPUserConfigurationsCollection class publicMethodsFor: 'ref:configurations'!

allCurrentConfigurations
	^Array new! !

!CMAPUserConfigurationsCollection publicMethodsFor: 'ref:accessing'!

name
	^'Configuraciones de Usuarios CMAP' copy! !

!CMAPUserConfigurationsCollection publicMethodsFor: 'svce'!

userConfigurationForUserName: theUserName

	| aUserName someConfigurations aUserConfiguration |
	(theUserName isNil or: [ theUserName isEmpty]) ifTrue: [ ^nil].

	aUserName := theUserName trimBlanks asUppercase.
	aUserName isEmpty ifTrue:  [ ^nil].

	someConfigurations := self configurations.
	(someConfigurations isNil or: [ someConfigurations isEmpty]) ifTrue: [ ^nil].
	
	aUserConfiguration := someConfigurations detect: [:aUserConfig | | aConfigUserName |
		aConfigUserName := aUserConfig userName.
		aConfigUserName isNil not and: [ aConfigUserName = aUserName]
	] ifNone: [ nil].
	^aUserConfiguration!

userNames

	|  someConfigurations someUserNames |

	someConfigurations := self configurations.
	(someConfigurations isNil or: [ someConfigurations isEmpty]) ifTrue: [ ^nil].
	
	someUserNames := OrderedCollection new: someConfigurations size.
	someConfigurations do: [:aUserConfig | | aUserName |
		aUserName := aUserConfig userName.
		(aUserName isNil not and: [ aUserName isEmpty not]) ifTrue: [ 
			someUserNames add: aUserName asUppercase
		]
	].

	^someUserNames! !

!CMAPUserSession class publicMethodsFor: 'instance creation'!

newForLoginSession: theLoginSession withUserConfiguration: theUserConfiguration	

	| aUserSession |
	theLoginSession isNil ifTrue: [ ^nil].
	theUserConfiguration isNil ifTrue: [ ^nil].

	aUserSession := self new.
	aUserSession forLoginSession: theLoginSession withUserConfiguration: theUserConfiguration.
	^aUserSession! !

!CMAPUserSession publicMethodsFor: 'configurations'!

applicationConfigurationsCollection
	applicationConfigurationsCollection isNil ifTrue: [ self initApplicationConfigurationsCollection].
	^applicationConfigurationsCollection!

initApplicationConfigurationsCollection
	applicationConfigurationsCollection := CMAPApplicationConfigurationsCollection exampleApplicationConfigurationsCollection01! !

!CMAPUserSession publicMethodsFor: 'initialize-release'!

forLoginSession: theLoginSession withUserConfiguration: theUserConfiguration	

	loginSession := theLoginSession.
	userConfiguration := theUserConfiguration! !

!CMAPUserSession publicMethodsFor: 'sessions'!

addApplicationSession: theApplicationSession
	| someApplicationSessions |
	theApplicationSession isNil ifTrue: [ ^self].

	someApplicationSessions := self userSessions.
	(someApplicationSessions isNil or: [ someApplicationSessions isEmpty]) ifTrue: [ ^self].

	(someApplicationSessions includes: theApplicationSession) ifTrue: [ ^self].
	someApplicationSessions add: theApplicationSession.

	self changed: #applicationSessions!

applicationSessions
	applicationSessions isNil ifTrue: [ applicationSessions := OrderedCollection new: 16].
	^applicationSessions!

loginSession
	^loginSession! !

!CMAPUserSession publicMethodsFor: 'svce'!

aboutToClose: theApplicationSession

	| someApplicationSessions |
	theApplicationSession isNil ifTrue: [ ^self].

	someApplicationSessions := self userSessions.
	(someApplicationSessions isNil or: [ someApplicationSessions isEmpty]) ifTrue: [ ^self].

	someApplicationSessions remove: theApplicationSession ifAbsent: [ ^self].
	self changed: #applicationSessions!

applicationNames

	| aApplicationConfigurationsCollection |

	aApplicationConfigurationsCollection := self applicationConfigurationsCollection.
	aApplicationConfigurationsCollection isNil ifTrue: [ ^nil].

	^aApplicationConfigurationsCollection applicationNames.!

logout

	self logoutAllowed ifFalse: [ ^false].

	self logoutUnconditionally.
	^true!

logoutAllowed

	| someApplicationSessions |

	someApplicationSessions := self applicationSessions.
	(someApplicationSessions isNil or: [ someApplicationSessions isEmpty]) ifTrue: [ ^true].

	^(someApplicationSessions 
		detect: [:anApplicationSession | anApplicationSession closeAllowed not]
		ifNone: [ nil]) isNil!

logoutUnconditionally

	| someApplicationSessions aLoginSession |

	aLoginSession := self loginSession.
	aLoginSession isNil ifFalse: [ 
		aLoginSession aboutToLogout: self
	].

	someApplicationSessions := self applicationSessions.
	(someApplicationSessions isNil or: [ someApplicationSessions isEmpty]) ifFalse: [
		someApplicationSessions copy do: [:anApplicationSession |
			anApplicationSession closeUnconditionally
		]
	].

	self release.!

openApplicationNamed: theApplicationName

	| aApplicationConfigurationsCollection aApplicationConfiguration aApplicationSession |

	theApplicationName isNil ifTrue:  [ ^nil].

	aApplicationConfigurationsCollection := self userConfigurationsCollection.
	aApplicationConfigurationsCollection isNil ifTrue: [ ^nil].

	aApplicationConfiguration := aApplicationConfigurationsCollection applicationConfigurationForApplicationName: theApplicationName.
	aApplicationConfiguration isNil ifTrue:  [ ^nil].
	
	aApplicationSession := CMAPApplicationSession newForUserSession: self withApplicationConfiguration: aApplicationConfiguration.
	aApplicationSession isNil ifTrue:  [ ^nil].

	self addApplicationSession: aApplicationSession.
	^aApplicationSession! !

!METAParameterConfigurationsCollection class publicMethodsFor: 'navigation'!

xstoreSelectors

	"METAChildSpecAutoViewEditor openOn: METAParameterConfigurationsCollection selector: #storeSelectors target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 4)
		add: (METATerminalChildSpec new
			name: 'Name';
			basicSelector: #name;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Name of the Configuration';
			displaySelector: nil;
			storeTagName: 'name';
			yourself);
		add: (METATerminalChildSpec new
			name: 'ClassName';
			basicSelector: #className;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Name of the Class to instantiate the Parameter';
			displaySelector: nil;
			storeTagName: 'classname';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Type';
			basicSelector: #type;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Type';
			displaySelector: nil;
			storeTagName: 'type';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Description';
			basicSelector: #description;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Description of the Parameter';
			displaySelector: nil;
			storeTagName: 'description';
			yourself);
		add: (METATerminalChildSpec new
			name: 'Value';
			basicSelector: #value;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Value';
			displaySelector: nil;
			storeTagName: 'value';
			yourself);
		yourself! !

CMAPDefinitionsHolderFactory initializeAfterLoad!
CMAPManager initializeAfterLoad!
CMAPProject initializeAfterLoad!
CMAPService initializeAfterLoad!
CMAPSession initializeAfterLoad!
CMAPApplicationSession initializeAfterLoad!
CMAPLoginSession initializeAfterLoad!
CMAPUserSession initializeAfterLoad!
CMAPInfoHolder initializeAfterLoad!
CMAPMetaInfoHolder initializeAfterLoad!
CMAPDefinitionsHolder initializeAfterLoad!
CMAPEditorsOpener initializeAfterLoad!
CMAPMETAConfiguration initializeAfterLoad!
CMAPApplicationConfiguration initializeAfterLoad!
CMAPDeveloperConfiguration initializeAfterLoad!
CMAPLoginConfiguration initializeAfterLoad!
CMAPUserConfiguration initializeAfterLoad!
CMAPApplicationConfigurationsCollection initializeAfterLoad!
CMAPLoginConfigurationsCollection initializeAfterLoad!
CMAPUserConfigurationsCollection initializeAfterLoad!
CMAPConfigurationsCollection initializeAfterLoad!
CMAPLauncherPanel initializeAfterLoad!
CMAPPilotMessagesPanel initializeAfterLoad!
CMAPPilotWindowsPanel initializeAfterLoad!
CMAPPathFinder initializeAfterLoad!
CMAPConfigurationsBrowser initializeAfterLoad!
CMAPApplicationBrowser initializeAfterLoad!
CMAPPilot initializeAfterLoad!
CODE_META_APP initializeAfterLoad!
CMAPTranslationHolder initializeAfterLoad!

CODE_META_APP loaded!
