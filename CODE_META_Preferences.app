'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Preferences in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Preferences becomeDefault!

METAPreferences subclass: #CMPreferences
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Preferences becomeDefault!

SubApplication subclass: #CODE_META_Preferences
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Preferences becomeDefault!

!CMPreferences class publicMethodsFor: 'CM-preferences'!

preferredAdaptorClass

	^CMAdaptor!

preferredAdaptorFactoryClass

	^CMAdaptorFactory!

preferredAdaptorFactoryFinderClass

	^CMAdaptorFactoryFinder!

preferredAttributeAdaptorClass

	^CMAttributeAdaptor!

preferredAttributeAdaptorFactoryClass

	^CMAttributeAdaptorFactory!

preferredAttributeAdaptorFactoryFinderClass

	^CMAttributeAdaptorFactoryFinder!

preferredAttributeChildrenNodesFactoryClass

	^CMAttributeChildrenNodesFactory!

preferredAttributeNodeClass

	^CMTerminalNode!

preferredAttributeNodeFactoryClass

	^CMAttributeNodeFactory!

preferredAttributeNodeFactoryFinderClass

	^CMAttributeNodeFactoryFinder!

preferredChildrenNodesFactoryClass

	^CMChildrenNodesFactory!

preferredChildrenNodesFactoryFinderClass

	^CMChildrenNodesFactoryFinder!

preferredClassAdaptorClass

	^CMClassAdaptor!

preferredClassAdaptorFactoryClass

	^CMClassAdaptorFactory!

preferredClassAdaptorFactoryFinderClass

	^CMClassAdaptorFactoryFinder!

preferredClassChildrenNodesFactoryClass

	^CMClassChildrenNodesFactory!

preferredClassChildrenNodesFactoryFinderClass

	^CMClassChildrenNodesFactoryFinder!

preferredClassNodeFactoryClass

	^CMClassNodeFactory!

preferredClassNodeFactoryFinderClass

	^CMClassNodeFactoryFinder!

preferredCMGenericObjectClass
	^CMGenericObject!

preferredCollectionAdaptorClass

	^CMCollectionAdaptor!

preferredCollectionAdaptorFactoryClass

	^CMCollectionAdaptorFactory!

preferredCollectionAdaptorFactoryFinderClass

	^CMCollectionAdaptorFactoryFinder!

preferredCollectionChildrenNodesFactoryClass

	^CMCollectionChildrenNodesFactory!

preferredCollectionChildrenNodesFactoryFinderClass

	^CMCollectionChildrenNodesFactoryFinder!

preferredCollectionNodeFactoryClass

	^CMCollectionNodeFactory!

preferredCollectionNodeFactoryFinderClass

	^CMCollectionNodeFactoryFinder!

preferredDependencyManagementClass

	^CMDependencyManagement!

preferredMetaInfoLikehoodEvaluator
	^CMMetaInfoLikehoodEvaluator!

preferredMetaInfoLikehoodEvaluatorClass

	^CMMetaInfoLikehoodEvaluator!

preferredObjectAdaptorClass

	^CMObjectAdaptor!

preferredObjectAdaptorFactoryClass

	^CMObjectAdaptorFactory!

preferredObjectAdaptorFactoryFinderClass

	^CMObjectAdaptorFactoryFinder!

preferredObjectChildrenNodesFactoryClass

	^CMObjectChildrenNodesFactory!

preferredObjectChildrenNodesFactoryFinderClass

	^CMObjectChildrenNodesFactoryFinder!

preferredObjectNodeClass

	^CMObjectNode!

preferredObjectNodeFactoryClass

	^CMObjectNodeFactory!

preferredObjectNodeFactoryFinderClass

	^CMObjectNodeFactoryFinder!

preferredOrderedAdaptorClass

	^CMOrderedAdaptor!

preferredOrderedAdaptorFactoryClass

	^CMOrderedAdaptorFactory!

preferredOrderedAdaptorFactoryFinderClass

	^CMOrderedAdaptorFactoryFinder!

preferredOrderedNodeClass

	^CMOrderedNode!

preferredOrderedNodeFactoryClass

	^CMOrderedNodeFactory!

preferredOrderedNodeFactoryFinderClass

	^CMOrderedNodeFactoryFinder!

preferredRootAdaptorClass

	^CMRootAdaptor!

preferredRootAdaptorFactoryClass

	^CMRootAdaptorFactory!

preferredRootAdaptorFactoryFinderClass

	^CMRootAdaptorFactoryFinder!

preferredRootChildrenNodesFactoryClass

	^CMRootChildrenNodesFactory!

preferredRootChildrenNodesFactoryFinderClass

	^CMRootChildrenNodesFactoryFinder!

preferredRootNodeClass

	^CMRootNode!

preferredRootNodeFactoryClass

	^CMRootNodeFactory!

preferredRootNodeFactoryFinderClass

	^CMRootNodeFactoryFinder!

preferredRootNodesManagerClass

	^CMRootNodesManager!

preferredTransactionClass
	^CMTransaction!

preferredTransactionNesterClass
	^CMTransactionNester!

preferredViewMetaInfoUtilsClass

	^CMViewMetaInfoUtils!

preferredViewRootNodeFactory
	^CMNodeFactory! !

!CMPreferences class publicMethodsFor: 'META-preferences'!

preferredCMAspectAdaptorWithCheckClass

	^CMAspectAdaptorWithCheck!

preferredCMClassAspectAdaptorWithCheckClass

	^CMClassAspectAdaptorWithCheck!

preferredCMCodeAspectAdaptorWithCheckClass

	^CMCodeAspectAdaptorWithCheck!

preferredCMEnumAspectAdaptorWithCheckClass

	^CMEnumAspectAdaptorWithCheck!

preferredCMOperationAdaptorWithCheckClass

	^CMOperationAdaptor!

preferredCMTerminalAdaptorWithCheckClass

	^CMTerminalAspectAdaptorWithCheck! !

!CMPreferences class publicMethodsFor: 'preferences'!

preferredClassNodeClass

	^CMClassNode!

preferredCMClassChildSpecClass
	

	^CMClassChildSpec!

preferredCMCollectionChildSpecClass
	

	^CMCollectionChildSpec!

preferredCMOperationAdaptorClass
	

	^CMOperationAdaptor!

preferredCMOperationVoidNoArgsChildSpecClass
	

	^CMOperationVoidNoArgsChildSpec!

preferredCMOrderedCollectionChildSpecClass
	

	^CMOrderedCollectionChildSpec!

preferredCMPerspectiveSpecClass
	

	^CMPerspectiveSpec!

preferredCMTerminalChildSpecClass
	

	^CMTerminalChildSpec!

preferredCMTerminalCollectionChildSpecClass
	

	^CMTerminalOrderedCollectionChildSpec!

preferredCMTerminalOrderedCollectionChildSpecClass
	

	^CMTerminalOrderedCollectionChildSpec!

preferredCMTextChildSpecClass
	

	^CMTextChildSpec!

preferredCollectionNodeClass

	^CMCollectionNode!

preferredDefinitionsHolderClass

	^CMDefinitionsHolder!

preferredPreferencesClass

	^self!

preferredSimpleFilterClass
	^CMSimpleFilter!

preferredSimpleFilterSpecClass
	^CMSimpleFilterSpec!

preferredSimpleTestClass
	^CMSimpleTest!

preferredSimpleTestSpecClass
	^CMSimpleTestSpec!

preferredTerminalInCollectionNodeClass
	

	^CMTerminalInCollectionNode!

preferredTreeHolderNodeClass

	^CMTreeHolderNode!

preferredTreeListNodeClass

	^CMTreeListNode! !

!CMPreferences class publicMethodsFor: 'preferences-refin'!

preferredApplicationBrowserClass
	^CODEMModelGenericBrowser!

preferredPathFinderApplicationBrowserClass
	^CODEMModelPathFinderGenericBrowser! !

CMPreferences initializeAfterLoad!
CODE_META_Preferences initializeAfterLoad!

CODE_META_Preferences loaded!
