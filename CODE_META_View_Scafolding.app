'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:54 pm'!



((CODE_META_View createSubApplication: #CODE_META_View_Scafolding in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_View_Scafolding becomeDefault!

CMViewCollaborator subclass: #CMAdaptorFactoryFinder
	instanceVariableNames: 'adaptorFactories '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMAdaptorFactoryFinder subclass: #CMFeatureAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMFeatureAdaptorFactoryFinder subclass: #CMAttributeAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMFeatureAdaptorFactoryFinder subclass: #CMClassAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMFeatureAdaptorFactoryFinder subclass: #CMCollectionAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMFeatureAdaptorFactoryFinder subclass: #CMOrderedAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMAdaptorFactoryFinder subclass: #CMVirtualObjectAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMVirtualObjectAdaptorFactoryFinder subclass: #CMObjectAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMVirtualObjectAdaptorFactoryFinder subclass: #CMRootAdaptorFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMViewCollaborator subclass: #CMChildrenNodesFactoryFinder
	instanceVariableNames: 'childrenNodeFactories '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMChildrenNodesFactoryFinder subclass: #CMClassChildrenNodesFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMChildrenNodesFactoryFinder subclass: #CMCollectionChildrenNodesFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMChildrenNodesFactoryFinder subclass: #CMObjectChildrenNodesFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMChildrenNodesFactoryFinder subclass: #CMOrderedChildrenNodesFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMChildrenNodesFactoryFinder subclass: #CMRootChildrenNodesFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMViewCollaborator subclass: #CMNodeFactoryFinder
	instanceVariableNames: 'nodeFactories '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMNodeFactoryFinder subclass: #CMAttributeNodeFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMNodeFactoryFinder subclass: #CMClassNodeFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMNodeFactoryFinder subclass: #CMCollectionNodeFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMNodeFactoryFinder subclass: #CMObjectNodeFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMNodeFactoryFinder subclass: #CMOrderedNodeFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

CMNodeFactoryFinder subclass: #CMRootNodeFactoryFinder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

SubApplication subclass: #CODE_META_View_Scafolding
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View_Scafolding becomeDefault!

!CMAdaptorFactoryFinder publicMethodsFor: 'accessing'!

adaptorFactories
	adaptorFactories isNil ifTrue: [ self initAdaptorFactories].
	^adaptorFactories!

adaptorFactoriesAdd: theAdaptorFactory
	self adaptorFactories add: theAdaptorFactory! !

!CMAdaptorFactoryFinder publicMethodsFor: 'initialize-release'!

initAdaptorFactories
	adaptorFactories := OrderedCollection new: 3! !

!CMAttributeAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactoryInView: theView
	| anAdaptorFactory anAdaptorFactoryClass |
	anAdaptorFactoryClass := nil.
	theView isNil ifFalse: [ 
		anAdaptorFactory := theView attributeAdaptorFactory.
		anAdaptorFactory isNil ifFalse: [ ^anAdaptorFactory].

		anAdaptorFactoryClass := theView attributeAdaptorFactoryClass
	].

	anAdaptorFactoryClass isNil ifTrue: [ anAdaptorFactoryClass := self preferredAttributeAdaptorFactoryClass].

	anAdaptorFactory := anAdaptorFactoryClass new.
	self adaptorFactoriesAdd: anAdaptorFactory.
	^anAdaptorFactory! !

!CMAttributeNodeFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultNodeFactory
	| aFactory |
	aFactory := self preferredAttributeNodeFactoryClass new.
	self nodeFactoriesAdd: aFactory.
	^aFactory! !

!CMChildrenNodesFactoryFinder publicMethodsFor: 'accessing'!

childrenNodeFactories
	childrenNodeFactories isNil ifTrue: [ self initChildrenNodeFactories].
	^childrenNodeFactories!

childrenNodeFactoriesAdd: theChildrenNodeFactory
	self childrenNodeFactories add: theChildrenNodeFactory! !

!CMChildrenNodesFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultChildrenNodesFactory
	self subclassResponsibility! !

!CMChildrenNodesFactoryFinder publicMethodsFor: 'initialize-release'!

initChildrenNodeFactories
	childrenNodeFactories := OrderedCollection new: 3! !

!CMClassAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactoryInView: theView
	| anAdaptorFactory anAdaptorFactoryClass |
	anAdaptorFactoryClass := nil.
	theView isNil ifFalse: [ 
		anAdaptorFactory := theView classAdaptorFactory.
		anAdaptorFactory isNil ifFalse: [ ^anAdaptorFactory].

		anAdaptorFactoryClass := theView classAdaptorFactoryClass
	].

	anAdaptorFactoryClass isNil ifTrue: [ anAdaptorFactoryClass := self preferredClassAdaptorFactoryClass].

	anAdaptorFactory := anAdaptorFactoryClass new.
	self adaptorFactoriesAdd: anAdaptorFactory.
	^anAdaptorFactory! !

!CMClassChildrenNodesFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultChildrenNodesFactory
	| aFactory |
	aFactory := self preferredClassChildrenNodesFactoryClass new.
	self childrenNodeFactoriesAdd: aFactory.
	^aFactory! !

!CMClassChildrenNodesFactoryFinder publicMethodsFor: 'svces'!

findChildrenNodesFactoryForNode: theNode  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo 

	| someFactories aChildrenNodeFactory  aDefaultFactory |

	theNode isNil ifTrue: [ ^nil].

	someFactories := self childrenNodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultChildrenNodesFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aChildrenNodesFactoryFinder childrenNodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aChildrenNodeFactory := someFactories detect: [:aFactory |
		aFactory isChildrenNodeFactory: theViewMetaInfo forNode: theNode] ifNone: [ nil].

	^aChildrenNodeFactory! !

!CMClassNodeFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultNodeFactory
	| aFactory |
	aFactory := self preferredClassNodeFactoryClass new.
	self nodeFactoriesAdd: aFactory.
	^aFactory! !

!CMCollectionAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactoryInView: theView
	| anAdaptorFactory anAdaptorFactoryClass |
	anAdaptorFactoryClass := nil.
	theView isNil ifFalse: [ 
		anAdaptorFactory := theView collectionAdaptorFactory.
		anAdaptorFactory isNil ifFalse: [ ^anAdaptorFactory].

		anAdaptorFactoryClass := theView collectionAdaptorFactoryClass
	].

	anAdaptorFactoryClass isNil ifTrue: [ anAdaptorFactoryClass := self preferredCollectionAdaptorFactoryClass].

	anAdaptorFactory := anAdaptorFactoryClass new.
	self adaptorFactoriesAdd: anAdaptorFactory.
	^anAdaptorFactory! !

!CMCollectionChildrenNodesFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultChildrenNodesFactory
	| aFactory |
	aFactory := self preferredCollectionChildrenNodesFactoryClass new.
	self childrenNodeFactoriesAdd: aFactory.
	^aFactory! !

!CMCollectionChildrenNodesFactoryFinder publicMethodsFor: 'svces'!

findChildrenNodesFactoryForNode: theNode  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo 

	| someFactories aChildrenNodeFactory  aDefaultFactory |

	theNode isNil ifTrue: [ ^nil].

	someFactories := self childrenNodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultChildrenNodesFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aChildrenNodesFactoryFinder childrenNodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aChildrenNodeFactory := someFactories detect: [:aFactory |
		aFactory isChildrenNodeFactory: theViewMetaInfo forNode: theNode] ifNone: [ nil].

	^aChildrenNodeFactory! !

!CMCollectionNodeFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultNodeFactory
	| aFactory |
	aFactory := self preferredCollectionNodeFactoryClass new.
	self nodeFactoriesAdd: aFactory.
	^aFactory! !

!CMFeatureAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactoryInView: theView
	self subclassResponsibility! !

!CMFeatureAdaptorFactoryFinder publicMethodsFor: 'svces'!

findAdaptorFactoryWithViewMetaInfo: theViewMetaInfo elementMap: theElementMap inView: theView

	| someFactories aDefaultFactory anAdaptorFactory |

	theViewMetaInfo isNil ifTrue: [ ^nil].

	someFactories := self adaptorFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultAdaptorFactoryInView: theView.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aAttributeAdaptorFactoryFinder adaptorFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	anAdaptorFactory := someFactories detect: [:aFactory |
		aFactory isAdaptorFactoryForViewMetaInfo: theViewMetaInfo elementMap: theElementMap] ifNone: [ nil].

	^anAdaptorFactory! !

!CMNodeFactoryFinder publicMethodsFor: 'accessing'!

nodeFactories
	nodeFactories isNil ifTrue: [ self initNodeFactories].
	^nodeFactories!

nodeFactoriesAdd: theRootNodeFactory
	self nodeFactories add: theRootNodeFactory! !

!CMNodeFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultNodeFactory
	self subclassResponsibility! !

!CMNodeFactoryFinder publicMethodsFor: 'initialize-release'!

initNodeFactories
	nodeFactories := OrderedCollection new: 3! !

!CMNodeFactoryFinder publicMethodsFor: 'svces'!

findNodeFactory: theAttribute forNode: theNode

	| someFactories aDefaultFactory anAttributeNodeFactory |

	theNode isNil ifTrue: [ ^nil].

	someFactories := self nodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultNodeFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'a', self class name, ' nodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	anAttributeNodeFactory := someFactories detect: [:aFactory |
		aFactory isNodeFactory: theAttribute  forNode: theNode ] ifNone: [ nil].

	^anAttributeNodeFactory!

findNodeFactoryForObject: theObject inView: theView

	| someFactories aRootNodeFactory  aDefaultFactory |
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	someFactories := self nodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultNodeFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactoryFinder rootNodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aRootNodeFactory := someFactories detect: [:aFactory |
		aFactory isNodeFactoryForObject: theObject  inView: theView] ifNone: [ nil].

	^aRootNodeFactory!

findNodeFactoryForObject: theObject 
	withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap withSourceMetaInfo: theSourceMetaInfo
	inView: theView

	^self findNodeFactoryForObject: theObject inView: theView! !

!CMObjectAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactory
	| aFactory |
	aFactory := self preferredObjectAdaptorFactoryClass new.
	self adaptorFactoriesAdd: aFactory.
	^aFactory! !

!CMObjectChildrenNodesFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultChildrenNodesFactory
	| aFactory |
	aFactory := self preferredObjectChildrenNodesFactoryClass new.
	self childrenNodeFactoriesAdd: aFactory.
	^aFactory! !

!CMObjectChildrenNodesFactoryFinder publicMethodsFor: 'svces'!

findChildrenNodesFactoryForNode: theNode  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo 

	| someFactories aChildrenNodeFactory  aDefaultFactory |

	theNode isNil ifTrue: [ ^nil].

	someFactories := self childrenNodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultChildrenNodesFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aChildrenNodesFactoryFinder childrenNodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aChildrenNodeFactory := someFactories detect: [:aFactory |
		aFactory isChildrenNodeFactory: theViewMetaInfo forNode: theNode] ifNone: [ nil].

	^aChildrenNodeFactory!

findChildrenNodesFactoryForObject: theObject 
	withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap withSourceMetaInfo: theSourceMetaInfo
	inView: theView

	| someFactories aChildrenNodeFactory  aDefaultFactory |

	theObject isNil ifTrue: [ ^nil].

	someFactories := self childrenNodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultChildrenNodesFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aChildrenNodesFactoryFinder childrenNodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aChildrenNodeFactory := someFactories detect: [:aFactory |
		aFactory isNodeFactoryForObject: theObject  inView: theView] ifNone: [ nil].

	^aChildrenNodeFactory! !

!CMObjectNodeFactoryFinder publicMethodsFor: 'private'!

buildDefaultNodeFactory
	| aFactory |
	aFactory := self preferredObjectNodesFactoryClass new.
	self nodeFactoriesAdd: aFactory.
	^aFactory! !

!CMObjectNodeFactoryFinder publicMethodsFor: 'svces'!

findNodeFactoryForObject: theObject 
	withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap withSourceMetaInfo: theSourceMetaInfo
	inView: theView

	^self findNodeFactoryForObject: theObject inView: theView! !

!CMOrderedAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactoryInView: theView
	| anAdaptorFactory anAdaptorFactoryClass |
	anAdaptorFactoryClass := nil.
	theView isNil ifFalse: [ 
		anAdaptorFactory := theView orderedAdaptorFactory.
		anAdaptorFactory isNil ifFalse: [ ^anAdaptorFactory].

		anAdaptorFactoryClass := theView orderedAdaptorFactoryClass
	].

	anAdaptorFactoryClass isNil ifTrue: [ anAdaptorFactoryClass := self preferredOrderedAdaptorFactoryClass].

	anAdaptorFactory := anAdaptorFactoryClass new.
	self adaptorFactoriesAdd: anAdaptorFactory.
	^anAdaptorFactory! !

!CMOrderedChildrenNodesFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultChildrenNodesFactory
	| aFactory |
	aFactory := self preferredOrderedChildrenNodesFactoryClass new.
	self childrenNodeFactoriesAdd: aFactory.
	^aFactory! !

!CMOrderedChildrenNodesFactoryFinder publicMethodsFor: 'svces'!

findChildrenNodesFactoryForNode: theNode  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo 

	| someFactories aChildrenNodeFactory  aDefaultFactory |

	theNode isNil ifTrue: [ ^nil].

	someFactories := self childrenNodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultChildrenNodesFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aChildrenNodesFactoryFinder childrenNodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aChildrenNodeFactory := someFactories detect: [:aFactory |
		aFactory isChildrenNodeFactory: theViewMetaInfo forNode: theNode] ifNone: [ nil].

	^aChildrenNodeFactory! !

!CMOrderedNodeFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultNodeFactory
	| aFactory |
	aFactory := self preferredOrderedNodeFactoryClass new.
	self nodeFactoriesAdd: aFactory.
	^aFactory! !

!CMRootAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactory
	| aFactory |
	aFactory := self preferredRootAdaptorFactoryClass new.
	self adaptorFactoriesAdd: aFactory.
	^aFactory! !

!CMRootChildrenNodesFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultChildrenNodesFactory
	| aFactory |
	aFactory := self preferredRootChildrenNodesFactoryClass new.
	self childrenNodeFactoriesAdd: aFactory.
	^aFactory! !

!CMRootChildrenNodesFactoryFinder publicMethodsFor: 'svces'!

findChildrenNodesFactoryForObject: theObject  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo inView: theView

	| someFactories aChildrenNodeFactory  aDefaultFactory |

	theObject isNil ifTrue: [ ^nil].

	someFactories := self childrenNodeFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultChildrenNodesFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aChildrenNodesFactoryFinder childrenNodeFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aChildrenNodeFactory := someFactories detect: [:aFactory |
		aFactory isChildrenNodeFactoryForObject: theObject inView: theView ] ifNone: [ nil].

	^aChildrenNodeFactory! !

!CMRootNodeFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultNodeFactory
	| aFactory |
	aFactory := self preferredRootNodeFactoryClass new.
	self nodeFactoriesAdd: aFactory.
	^aFactory! !

!CMVirtualObjectAdaptorFactoryFinder publicMethodsFor: 'initialize-collaborators'!

buildDefaultAdaptorFactory
	self subclassResponsibility! !

!CMVirtualObjectAdaptorFactoryFinder publicMethodsFor: 'svces'!

findAdaptorFactoryForObject: theObject  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo inView: theView

	| someFactories aAdaptorFactory  aDefaultFactory |

	theObject isNil ifTrue: [ ^nil].

	someFactories := self adaptorFactories.
	(someFactories isNil or: [ someFactories isEmpty]) ifTrue: [ 
		aDefaultFactory := self buildDefaultAdaptorFactory.
		aDefaultFactory isNil ifFalse: [ ^aDefaultFactory].
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aAdaptorFactoryFinder adaptorFactories isNil or isEmpty'
	].
	someFactories size = 1 ifTrue: [ ^someFactories first].
	aAdaptorFactory := someFactories detect: [:aFactory |
		aFactory isAdaptorFactoryForObject: theObject inView: theView ] ifNone: [ nil].

	^aAdaptorFactory! !

CMAdaptorFactoryFinder initializeAfterLoad!
CMFeatureAdaptorFactoryFinder initializeAfterLoad!
CMAttributeAdaptorFactoryFinder initializeAfterLoad!
CMClassAdaptorFactoryFinder initializeAfterLoad!
CMCollectionAdaptorFactoryFinder initializeAfterLoad!
CMOrderedAdaptorFactoryFinder initializeAfterLoad!
CMVirtualObjectAdaptorFactoryFinder initializeAfterLoad!
CMObjectAdaptorFactoryFinder initializeAfterLoad!
CMRootAdaptorFactoryFinder initializeAfterLoad!
CMChildrenNodesFactoryFinder initializeAfterLoad!
CMClassChildrenNodesFactoryFinder initializeAfterLoad!
CMCollectionChildrenNodesFactoryFinder initializeAfterLoad!
CMObjectChildrenNodesFactoryFinder initializeAfterLoad!
CMOrderedChildrenNodesFactoryFinder initializeAfterLoad!
CMRootChildrenNodesFactoryFinder initializeAfterLoad!
CMNodeFactoryFinder initializeAfterLoad!
CMAttributeNodeFactoryFinder initializeAfterLoad!
CMClassNodeFactoryFinder initializeAfterLoad!
CMCollectionNodeFactoryFinder initializeAfterLoad!
CMObjectNodeFactoryFinder initializeAfterLoad!
CMOrderedNodeFactoryFinder initializeAfterLoad!
CMRootNodeFactoryFinder initializeAfterLoad!
CODE_META_View_Scafolding initializeAfterLoad!

CODE_META_View_Scafolding loaded!
