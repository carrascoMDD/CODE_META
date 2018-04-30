'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_View in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_View becomeDefault!

Object subclass: #CMSignals
	instanceVariableNames: ''
	classVariableNames: 'ChildrenNodesFactoryNotFoundSignal CMQueryRecursionSignal CMSignal CMViewBadSetupSignal MapInfoNotFoundSignal MetaInfoForRootNodeNotFoundSignal MetaInfoForSourceObjectNotFoundSignal ModelAccessorNotFoundSignal NewRootNodeFailed NodeWithoutAccessorSignal NodeWithoutAdaptorSignal NodeWithoutChildrenFactory NodeWithoutChildrenNodesFactorySignal NodeWithoutViewMetaInfoSignal RootNodesFactoryClassNotFoundSignal RootNodesFactoryFinderNotFoundSignal RootNodesFactoryNotFoundSignal '
	poolDictionaries: ''!

CODE_META_View becomeDefault!

Object subclass: #CMViewCollaborator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMAdaptor
	instanceVariableNames: 'node '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAdaptor subclass: #CMAbstractObjectAdaptor
	instanceVariableNames: 'sourceObject featureAdaptors '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectAdaptor subclass: #CMObjectAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectAdaptor subclass: #CMRootAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAdaptor subclass: #CMFeatureAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptor subclass: #CMAttributeAdaptor
	instanceVariableNames: 'sourceValue '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptor subclass: #CMClassAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptor subclass: #CMCollectionAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptor subclass: #CMOrderedAdaptor
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAdaptorFactory subclass: #CMAbstractObjectAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectAdaptorFactory subclass: #CMObjectAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectAdaptorFactory subclass: #CMRootAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAdaptorFactory subclass: #CMFeatureAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptorFactory subclass: #CMAttributeAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptorFactory subclass: #CMClassAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptorFactory subclass: #CMCollectionAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureAdaptorFactory subclass: #CMOrderedAdaptorFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMChildrenNodesFactory subclass: #CMAbstractObjectChildrenNodesFactory
	instanceVariableNames: 'attributeNodeFactoryFinder classNodeFactoryFinder collectionNodeFactoryFinder orderedNodeFactoryFinder '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectChildrenNodesFactory subclass: #CMObjectChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectChildrenNodesFactory subclass: #CMRootChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMChildrenNodesFactory subclass: #CMFeatureChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureChildrenNodesFactory subclass: #CMAttributeChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureChildrenNodesFactory subclass: #CMFeatureReferencingObjectsChildrenNodesFactory
	instanceVariableNames: 'metaInfoLikehoodEvaluator nodeFactoryFinder adaptorFactoryFinder '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureReferencingObjectsChildrenNodesFactory subclass: #CMClassChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureReferencingObjectsChildrenNodesFactory subclass: #CMCollectionChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureReferencingObjectsChildrenNodesFactory subclass: #CMOrderedChildrenNodesFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMDependencyManagement
	instanceVariableNames: 'view '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMMetaInfoLikehoodEvaluator
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMModelMap
	instanceVariableNames: 'map '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMNodeFactory
	instanceVariableNames: 'childrenNodesFactoryFinder adaptorFactoryFinder '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMNodeFactory subclass: #CMAbstractObjectNodeFactory
	instanceVariableNames: 'metaInfoLikehoodEvaluator '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectNodeFactory subclass: #CMObjectNodeFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMAbstractObjectNodeFactory subclass: #CMRootNodeFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMNodeFactory subclass: #CMFeatureNodeFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureNodeFactory subclass: #CMAttributeNodeFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureNodeFactory subclass: #CMClassNodeFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureNodeFactory subclass: #CMCollectionNodeFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMFeatureNodeFactory subclass: #CMOrderedNodeFactory
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMRootNodesManager
	instanceVariableNames: 'view rootNodes rootNodesFactoryFinder '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMSourceModel
	instanceVariableNames: 'model '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMView
	instanceVariableNames: 'name viewModel sourceModel modelMap rootNodesManager attributeAdaptorFactoryClass attributeAdaptorFactory classAdaptorFactoryClass classAdaptorFactory collectionAdaptorFactoryClass collectionAdaptorFactory orderedAdaptorFactoryClass orderedAdaptorFactory dependencyManagement sourceObjectDeletedAspects '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMViewMetaInfoUtils
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

CMViewCollaborator subclass: #CMViewModel
	instanceVariableNames: 'model '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

SubApplication subclass: #CODE_META_View
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_View becomeDefault!

!CMAbstractObjectAdaptor class publicMethodsFor: 'instance creation'!

newAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo

	| anAdaptor |
	anAdaptor := self new.
	anAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anAdaptor!

newAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo
 inView: theView

	| anAdaptor |
	anAdaptor := self new.
	anAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anAdaptor! !

!CMAbstractObjectAdaptor publicMethodsFor: 'accessing'!

featureAdaptors
	featureAdaptors isNil ifTrue: [ self initFeatureAdaptors].
	^featureAdaptors!

sourceObject
	^sourceObject! !

!CMAbstractObjectAdaptor publicMethodsFor: 'initialize-release'!

initFeatureAdaptors
	featureAdaptors := OrderedCollection new: 6!

initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo

	sourceObject 	:= theObject!

release

	| someFeatureAdaptors |

	self disconnectFromObject.

	someFeatureAdaptors := self featureAdaptors.
	someFeatureAdaptors isNil ifFalse: [ 
		someFeatureAdaptors copy do: [:anAdaptor | anAdaptor release]
	].

	featureAdaptors := nil.
	sourceObject := nil.

	super release! !

!CMAbstractObjectAdaptor publicMethodsFor: 'private'!

connectToObject

	| aNode aView aDependencyManagement aSourceObject |
	aSourceObject := self sourceObject.
	aSourceObject isNil ifTrue: [ ^CMSignals objectAdaptorWithoutSourceObjectSignal raiseWith: self].

	aNode := self node.
	aNode isNil ifTrue: [ ^CMSignals objectAdaptorWithoutNodeSignal raiseWith: self].

	aView := aNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: aNode].

	aDependencyManagement := aView dependencyManagement.
	aDependencyManagement isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: aNode].

	aDependencyManagement addDependent: self toObject: aSourceObject!

disconnectFromObject

	| aNode aView aDependencyManagement aSourceObject |
	aSourceObject := self sourceObject.
	aSourceObject isNil ifTrue: [ ^CMSignals objectAdaptorWithoutSourceObjectSignal raiseWith: self].

	aNode := self node.
	aNode isNil ifTrue: [ ^CMSignals objectAdaptorWithoutNodeSignal raiseWith: self].

	aView := aNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: aNode].

	aDependencyManagement := aView dependencyManagement.
	aDependencyManagement isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: aNode].

	aDependencyManagement removeDependent: self fromObject: aSourceObject!

featureAdaptorsToUpdateForAspect: theAspect

	| someFeatureAdaptors someFeatureAdaptorsToUpdate |
	someFeatureAdaptors  := self featureAdaptors.
	someFeatureAdaptors isNil  ifTrue: [ ^nil].

	someFeatureAdaptorsToUpdate := someFeatureAdaptors select: [:anAdaptor | anAdaptor shouldUpdateForAspect: theAspect ].
	^someFeatureAdaptorsToUpdate! !

!CMAbstractObjectAdaptor publicMethodsFor: 'services'!

addFeatureAdaptor: theAdaptor
	theAdaptor isNil ifTrue: [ ^self].

	self featureAdaptors add: theAdaptor! !

!CMAbstractObjectAdaptor publicMethodsFor: 'testing'!

isReleased
	^super isReleased or: [sourceObject isNil]! !

!CMAbstractObjectAdaptor publicMethodsFor: 'updating'!

syncSourceObjectHasBeenDeleted

	|  aNode someRemovedNodes aParentNode someExistingChildren |

	aNode := self node.
	aNode isNil ifTrue: [ ^nil].

	someRemovedNodes := OrderedCollection new: 8.
	aParentNode := aNode parent.
	aParentNode isNil ifFalse: [ 
		someExistingChildren := aParentNode existingChildren.
		someExistingChildren isNil ifFalse: [ someExistingChildren remove: aNode ifAbsent: [ nil]]
	].
		
	aNode recursivelyRemoveChildrenRemovedOnCollection: someRemovedNodes.
	aParentNode isNil ifFalse: [ 
		aParentNode changed: #children
	]!

update: theAspect with: theArgument from: theObject

	| aNode aView someSourceObjectDeletedAspects someFeatureAdaptors |
 
	self isReleased ifTrue: [ ^self].

	aNode := self node.
	aNode isNil ifTrue: [ CMSignals adaptorWithoutNodeSignal raiseWith: self].

	aView := aNode view.
	aView isNil ifTrue: [ CMSignals nodeWithoutViewSignal raiseWith: aNode].
 
	someSourceObjectDeletedAspects := aView sourceObjectDeletedAspects.
	(someSourceObjectDeletedAspects includes: theAspect) ifTrue: [ 
		self syncSourceObjectHasBeenDeleted. 
		^self
	].

	someFeatureAdaptors  := self featureAdaptorsToUpdateForAspect: theAspect.
	someFeatureAdaptors isNil  ifTrue: [ ^self].
	someFeatureAdaptors do: [:anAdaptor |
		anAdaptor updateChildren
	].! !

!CMAbstractObjectAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo inView: theView

	self subclassResponsibility!

isAdaptorFactoryForObject: theObject inView: theView

	self subclassResponsibility! !

!CMAbstractObjectChildrenNodesFactory publicMethodsFor: 'collaborators'!

attributeNodeFactoryFinder
	attributeNodeFactoryFinder isNil ifTrue: [ self initAttributeNodeFactoryFinder].
	^attributeNodeFactoryFinder!

classNodeFactoryFinder
	classNodeFactoryFinder isNil ifTrue: [ self initClassNodeFactoryFinder].
	^classNodeFactoryFinder!

collectionNodeFactoryFinder
	collectionNodeFactoryFinder isNil ifTrue: [ self initCollectionNodeFactoryFinder].
	^collectionNodeFactoryFinder!

orderedNodeFactoryFinder
	orderedNodeFactoryFinder isNil ifTrue: [ self initOrderedNodeFactoryFinder].
	^orderedNodeFactoryFinder! !

!CMAbstractObjectChildrenNodesFactory publicMethodsFor: 'initialize-collaborators'!

initAttributeNodeFactoryFinder
	self subclassResponsibility!

initAttributeNodeFactoryFinder: theAttributeNodeFactoryFinder
	attributeNodeFactoryFinder := theAttributeNodeFactoryFinder!

initClassNodeFactoryFinder
	self subclassResponsibility!

initClassNodeFactoryFinder: theClassNodeFactoryFinder
	classNodeFactoryFinder := theClassNodeFactoryFinder!

initCollectionNodeFactoryFinder
	self subclassResponsibility!

initCollectionNodeFactoryFinder: theCollectionNodeFactoryFinder
	collectionNodeFactoryFinder := theCollectionNodeFactoryFinder!

initOrderedNodeFactoryFinder
	self subclassResponsibility!

initOrderedNodeFactoryFinder: theOrderedNodeFactoryFinder
	orderedNodeFactoryFinder := theOrderedNodeFactoryFinder! !

!CMAbstractObjectChildrenNodesFactory publicMethodsFor: 'private'!

newChildAttributeNode: theAttribute forNode: theNode

	| aNewAttributeNode anAttributeNodeFactoryFinder anAttributeNodeFactory |

	anAttributeNodeFactoryFinder := self attributeNodeFactoryFinder.
	anAttributeNodeFactoryFinder isNil ifTrue: [ ^CMSignals attributeNodeFactoryFinderNotFoundSignal raiseWith: theNode].
	
	anAttributeNodeFactory := anAttributeNodeFactoryFinder findNodeFactory: theAttribute forNode: theNode.
	anAttributeNodeFactory isNil ifTrue: [ ^CMSignals attributeNodeFactoryNotFoundSignal raiseWith: theNode].
 
	aNewAttributeNode := anAttributeNodeFactory newNode: theAttribute forNode: theNode.
	^aNewAttributeNode.!

newChildInheritedAttributeNode: theAttribute elementMap: theElementMap   forNode: theNode

	| aNewAttributeNode anAttributeNodeFactoryFinder anAttributeNodeFactory |

	anAttributeNodeFactoryFinder := self attributeNodeFactoryFinder.
	anAttributeNodeFactoryFinder isNil ifTrue: [ ^CMSignals attributeNodeFactoryFinderNotFoundSignal raiseWith: theNode].
	
	anAttributeNodeFactory := anAttributeNodeFactoryFinder findNodeFactory: theAttribute forNode: theNode.
	anAttributeNodeFactory isNil ifTrue: [ ^CMSignals attributeNodeFactoryNotFoundSignal raiseWith: theNode].

	aNewAttributeNode := anAttributeNodeFactory newInheritedNode: theAttribute elementMap: theElementMap  forNode: theNode.
	^aNewAttributeNode.!

newChildInheritedRelationshipNode: theRelationship elementMap: theElementMap   forNode: theNode

	| aNewRelationshipNode aRelationshipNodeFactoryFinder aRelationshipNodeFactory |

	aRelationshipNodeFactoryFinder := theRelationship isMultiplicityMany 
		ifFalse: [ self classNodeFactoryFinder]
		ifTrue: [
			theRelationship isOrdered 
				ifFalse: [ self collectionNodeFactoryFinder]
				ifTrue: [ self orderedNodeFactoryFinder]].
	aRelationshipNodeFactoryFinder isNil ifTrue: [ ^CMSignals attributeNodeFactoryFinderNotFoundSignal raiseWith: theNode].
	
	aRelationshipNodeFactory := aRelationshipNodeFactoryFinder findNodeFactory: theRelationship forNode: theNode.
	aRelationshipNodeFactory isNil ifTrue: [ ^CMSignals attributeNodeFactoryNotFoundSignal raiseWith: theNode].

	aNewRelationshipNode := aRelationshipNodeFactory newInheritedNode: theRelationship elementMap: theElementMap  forNode: theNode.
	^aNewRelationshipNode.!

newChildRelationshipNode: theRelationship forNode: theNode

	| aNewRelationshipNode aRelationshipNodeFactoryFinder aRelationshipNodeFactory |

	aRelationshipNodeFactoryFinder := theRelationship isMultiplicityMany 
		ifFalse: [ self classNodeFactoryFinder]
		ifTrue: [
			theRelationship isOrdered 
				ifFalse: [ self collectionNodeFactoryFinder]
				ifTrue: [ self orderedNodeFactoryFinder]].

	aRelationshipNodeFactoryFinder isNil ifTrue: [ ^CMSignals relationshipNodeFactoryFinderNotFoundSignal raiseWith: theNode].
	
	aRelationshipNodeFactory := aRelationshipNodeFactoryFinder findNodeFactory: theRelationship forNode: theNode.
	aRelationshipNodeFactory isNil ifTrue: [ ^CMSignals attributeNodeFactoryNotFoundSignal raiseWith: theNode].

	aNewRelationshipNode := aRelationshipNodeFactory newNode: theRelationship forNode: theNode.
	^aNewRelationshipNode.!

newChildrenNodesFromInheritedViewMetaInfo: theViewMetaInfo forNode: theNode

	| someNewChildNodes someAttributes  someRelationships aMap aView aModelMap anElementMap |

	theViewMetaInfo isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	someNewChildNodes := OrderedCollection new: 16.
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	aModelMap := aView modelMap.
	aModelMap isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView modelMap isNil'].

	aMap := aModelMap map.
	aMap isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aModelMap map isNil'].

	anElementMap := self preferredViewMetaInfoUtilsClass scanMapsFolder: aMap forElementMapWithViewMetaInfo: theViewMetaInfo forNode: theNode.
	anElementMap isNil ifTrue: [ ^CMSignals mapElementForInheritedViewMetaInfoNotFoundSignal raiseWith: theNode].

	someAttributes := theViewMetaInfo attributes.
	(someAttributes isNil not and: [ someAttributes isEmpty not]) ifTrue: [ 
		someAttributes do: [:anAttribute |  | anAttributeNode |
			anAttributeNode := self newChildInheritedAttributeNode: anAttribute elementMap: anElementMap forNode: theNode.
			anAttributeNode isNil ifFalse: [ someNewChildNodes add: anAttributeNode]
		]
	].

	someRelationships := theViewMetaInfo relationships.
	(someRelationships isNil not and: [ someRelationships isEmpty not]) ifTrue: [ 
		someRelationships do: [:aRelationship |   | aRelationshipNode |
			aRelationshipNode := self newChildInheritedRelationshipNode: aRelationship elementMap: anElementMap forNode: theNode.
			aRelationshipNode isNil ifFalse: [ someNewChildNodes add: aRelationshipNode]
		]
	].

	^someNewChildNodes!

newChildrenNodesFromViewMetaInfo: theViewMetaInfo forNode: theNode

	| someNewChildNodes someAttributes  someRelationships |

	theViewMetaInfo isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	someNewChildNodes := OrderedCollection new: 16.

	someAttributes := theViewMetaInfo attributes.
	(someAttributes isNil not and: [ someAttributes isEmpty not]) ifTrue: [ 
		someAttributes do: [:anAttribute |  | anAttributeNode |
			anAttributeNode := self newChildAttributeNode: anAttribute forNode: theNode.
			anAttributeNode isNil ifFalse: [ someNewChildNodes add: anAttributeNode]
		]
	].

	someRelationships := theViewMetaInfo relationships.
	(someRelationships isNil not and: [ someRelationships isEmpty not]) ifTrue: [ 
		someRelationships do: [:aRelationship |   | aRelationshipNode |
			aRelationshipNode := self newChildRelationshipNode: aRelationship forNode: theNode.
			aRelationshipNode isNil ifFalse: [ someNewChildNodes add: aRelationshipNode]
		]
	].

	^someNewChildNodes! !

!CMAbstractObjectChildrenNodesFactory publicMethodsFor: 'svces'!

isChildrenNodeFactoryForObject: theObject inView: theView

	theObject isNil ifFalse: [ ^false].
	theView isNil ifFalse: [ ^false].

	^true!

newChildrenNodesForNode: theNode

	| aViewMetaInfo someNewChildNodes someViewMetaInfos someNodes |
	theNode isNil ifTrue: [ ^nil].

	aViewMetaInfo := theNode viewMetaInfo.
	aViewMetaInfo isNil ifTrue: [  ^CMSignals nodeWithoutViewMetaInfoSignal raiseWith: self].

	someNewChildNodes := OrderedCollection new: 16.

	someViewMetaInfos := aViewMetaInfo withAllSuperTypes reverse copyWithout: aViewMetaInfo.
	(someViewMetaInfos isNil or: [ someViewMetaInfos isEmpty]) ifTrue: [  ^someNewChildNodes].

	someViewMetaInfos do: [:aViewMI |  
		someNodes := self newChildrenNodesFromInheritedViewMetaInfo: aViewMI forNode: theNode.
		someNodes isNil ifFalse: [ someNewChildNodes addAll: someNodes]
	].

	someNodes := self newChildrenNodesFromViewMetaInfo: aViewMetaInfo forNode: theNode.
	someNodes isNil ifFalse: [ someNewChildNodes addAll: someNodes].

	^someNewChildNodes! !

!CMAbstractObjectNodeFactory publicMethodsFor: 'collaborators'!

metaInfoLikehoodEvaluator
	metaInfoLikehoodEvaluator isNil ifTrue: [ self initMetaInfoLikehoodEvaluator].
	^metaInfoLikehoodEvaluator! !

!CMAbstractObjectNodeFactory publicMethodsFor: 'initialize-collaborators'!

initMetaInfoLikehoodEvaluator
	self subclassResponsibility!

initMetaInfoLikehoodEvaluator: theMetaInfoLikehoodEvaluator
	metaInfoLikehoodEvaluator := theMetaInfoLikehoodEvaluator! !

!CMAbstractObjectNodeFactory publicMethodsFor: 'private'!

findNodeClassForObject: theObject inView: theView

	self subclassResponsibility! !

!CMAbstractObjectNodeFactory publicMethodsFor: 'svces'!

isNodeFactoryForObject: theObject inView: theView

	self subclassResponsibility!

newNodeForObject: theObject inView: theView

	self subclassResponsibility! !

!CMAdaptor class publicMethodsFor: 'instance creation'!

newAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo
 inView: theView

	| anAdaptor |
	anAdaptor := self new.
	anAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anAdaptor!

newAdaptorWithViewMetaInfo: theViewMetaInfo elementMap: theElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptor |
	anAdaptor := self new.
	anAdaptor
		initForNode: theNode  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anAdaptor! !

!CMAdaptor publicMethodsFor: 'accessing'!

elementMap
	| aNode |
	aNode := self node.
	^aNode isNil ifTrue: [ nil] ifFalse: [ aNode elementMap]!

node
	^node!

sourceMetaInfo
	| aNode |
	aNode := self node.
	^aNode isNil ifTrue: [ nil] ifFalse: [ aNode sourceMetaInfo]!

viewMetaInfo
	| aNode |
	aNode := self node.
	^aNode isNil ifTrue: [ nil] ifFalse: [ aNode viewMetaInfo]! !

!CMAdaptor publicMethodsFor: 'initialize-release'!

release


	node := nil.
	super release! !

!CMAdaptor publicMethodsFor: 'private'!

connectToObject
	self subclassResponsibility! !

!CMAdaptor publicMethodsFor: 'svces'!

connectToNode: theNode

	theNode isNil ifTrue: [ ^self].
	node := theNode.

	self connectToObject! !

!CMAdaptor publicMethodsFor: 'testing'!

isReleased
	^node isNil! !

!CMAttributeAdaptor class publicMethodsFor: 'instance creation'!

newAdaptorForAttributeNode: theAttribute elementMap: theAttributeElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAttributeAdaptor |
	anAttributeAdaptor := self new.
	anAttributeAdaptor
		initForNode: theNode  withViewMetaInfo: theAttribute elementMap: theAttributeElementMap sourceMetaInfo: theSourceMetaInfo.
	^anAttributeAdaptor! !

!CMAttributeAdaptor publicMethodsFor: 'accessing'!

sourceValue 
	sourceValue isNil ifTrue: [ self retrieveValue].
	^sourceValue! !

!CMAttributeAdaptor publicMethodsFor: 'svces'!

retrieveValue

	| aNode aSourceObject aSourceMetaInfo aReengineeredInstVarName aValue aParentNode aParentAdaptor |
	aNode := self node.
	aNode isNil ifTrue: [ ^nil].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [ ^nil].

	aParentAdaptor := aParentNode adaptor.
	aParentAdaptor isNil ifTrue: [ ^nil].

	aSourceObject := aParentAdaptor sourceObject.
	aSourceObject isNil ifTrue: [ ^nil].
	
	aSourceMetaInfo := aNode sourceMetaInfo.
	aSourceMetaInfo isNil ifTrue: [ ^nil].

	aReengineeredInstVarName := aSourceMetaInfo reengineredInstVarName.
	aReengineeredInstVarName isNil ifTrue: [ ^nil].

	aValue := aSourceObject perform: aReengineeredInstVarName asSymbol.
	sourceValue := aValue.
	^aValue! !

!CMAttributeAdaptor publicMethodsFor: 'updating'!

updateChildren

	self retrieveValue! !

!CMAttributeAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorWithViewMetaInfo: theAttribute elementMap: theAttributeElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptorClass |
	theNode isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredAttributeAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorWithViewMetaInfo: theAttribute elementMap: theAttributeElementMap 
		sourceMetaInfo: theSourceMetaInfo forNode: theNode!

isAdaptorFactoryForMetaInfo: theAttribute elementMap: theElementMap 

	theAttribute isNil ifTrue: [ ^false].
	theElementMap isNil ifTrue: [ ^false].

	^true! !

!CMAttributeChildrenNodesFactory publicMethodsFor: 'svces'!

isChildrenNodeFactoryForObject: theObject inView: theView

	theObject isNil ifFalse: [ ^false].
	theView isNil ifFalse: [ ^false].

	^true!

newChildrenNodesForNode: theNode

	^Array new! !

!CMAttributeNodeFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	adaptorFactoryFinder := self preferredAttributeAdaptorFactoryFinderClass new!

initChildrenNodesFactoryFinder
	childrenNodesFactoryFinder := self preferredAttributeChildrenNodesFactoryFinderClass new! !

!CMAttributeNodeFactory publicMethodsFor: 'private'!

 findNodeClass: theAttribute  forNode: theNode

	theNode isNil ifTrue: [ ^nil].
	theAttribute isNil ifTrue: [ ^nil].

	^self preferredAttributeNodeClass! !

!CMAttributeNodeFactory publicMethodsFor: 'svces'!

isNodeFactory: theAttribute  forNode: theNode

	theNode isNil ifTrue: [ ^false].
	theAttribute isNil ifTrue: [ ^false].
	
	^true!

newInheritedNode: theAttribute elementMap: theElementMap forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anAttributeNodeClass anAttributeElementMap someSourceMetaInfos anAttributeNode |

	theNode isNil ifTrue: [ ^nil].
	theAttribute isNil ifTrue: [ ^nil].
	theElementMap isNil ifTrue: [ ^nil].

	anAttributeNodeClass := self findNodeClass: theAttribute forNode: theNode.
	anAttributeNodeClass isNil  ifTrue: [ ^CMSignals attributeNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anAttributeElementMap := self preferredViewMetaInfoUtilsClass scanElementMap: theElementMap forElementMapWithViewMetaInfo: theAttribute forNode: theNode.
	anAttributeElementMap isNil ifTrue: [ ^CMSignals attributeNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anAttributeElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals attributeNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.
	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theAttribute elementMap: anAttributeElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorWithViewMetaInfo: theAttribute elementMap: anAttributeElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anAttributeNode := anAttributeNodeClass new.

	anAttributeNode view: aView.
	anAttributeNode childrenNodesFactory: self preferredAttributeChildrenNodesFactoryClass new.
	anAttributeNode sourceMetaInfo: aSourceMetaInfo.
	anAttributeNode elementMap: anAttributeElementMap.
	anAttributeNode viewMetaInfo: theAttribute.
	anAttributeNode adaptor: anAdaptor.

	anAttributeNode forzeAllAccessesPending.

	^anAttributeNode!

newNode: theAttribute  forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anAttributeNodeClass anAttributeElementMap someSourceMetaInfos anAttributeNode |

	theNode isNil ifTrue: [ ^nil].
	theAttribute isNil ifTrue: [ ^nil].

	anAttributeNodeClass := self findNodeClass: theAttribute forNode: theNode.
	anAttributeNodeClass isNil  ifTrue: [ ^CMSignals attributeNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anAttributeElementMap := self preferredViewMetaInfoUtilsClass scanNode: theNode forElementMapWithViewMetaInfo: theAttribute.
	anAttributeElementMap isNil ifTrue: [ ^CMSignals attributeNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anAttributeElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals attributeNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.
	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theAttribute elementMap: anAttributeElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorWithViewMetaInfo: theAttribute elementMap: anAttributeElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anAttributeNode := anAttributeNodeClass new.

	anAttributeNode view: aView.
	anAttributeNode childrenNodesFactory: self preferredAttributeChildrenNodesFactoryClass new.
	anAttributeNode sourceMetaInfo: aSourceMetaInfo.
	anAttributeNode elementMap: anAttributeElementMap.
	anAttributeNode viewMetaInfo: theAttribute.
	anAttributeNode adaptor: anAdaptor.

	anAttributeNode forzeAllAccessesPending.

	^anAttributeNode! !

!CMChildrenNodesFactory publicMethodsFor: 'svces'!

isChildrenNodeFactoryForObject: theObject inView: theView

	self subclassResponsibility! !

!CMClassAdaptor class publicMethodsFor: 'instance creation'!

newAdaptorForClassNode: theClass elementMap: theClassElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| aClassAdaptor |
	aClassAdaptor := self new.
	aClassAdaptor
		initForNode: theNode  withViewMetaInfo: theClass elementMap: theClassElementMap sourceMetaInfo: theSourceMetaInfo.
	^aClassAdaptor! !

!CMClassAdaptor publicMethodsFor: 'svces'!

retrieveRelatedObjects

	| aNode aSourceObject aSourceMetaInfo aReengineeredInstVarName anObject aParentNode aParentAdaptor |

	aNode := self node.
	aNode isNil ifTrue: [ ^nil].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [ ^nil].

	aParentAdaptor := aParentNode adaptor.
	aParentAdaptor isNil ifTrue: [ ^nil].

	aSourceObject := aParentAdaptor sourceObject.
	aSourceObject isNil ifTrue: [ ^nil].
	
	aSourceMetaInfo := aNode sourceMetaInfo.
	aSourceMetaInfo isNil ifTrue: [ ^nil].

	aReengineeredInstVarName := aSourceMetaInfo reengineredInstVarName.
	aReengineeredInstVarName isNil ifTrue: [ ^nil].

	anObject := aSourceObject perform: aReengineeredInstVarName asSymbol.
	^anObject! !

!CMClassAdaptor publicMethodsFor: 'updating'!

updateChildren

	| aNode someAddedNodes someRemovedNodes someExistingChildren someDeltaNodes aChildValue anExistingChildNode anExistingChildSourceObject anExistingChildNodeAdaptor aRemovedNode anAddedNode |
 
	self isReleased ifTrue: [ ^self].

	aNode := self node.
	aNode isNil ifTrue: [ ^false].

	someExistingChildren := aNode existingChildren.

	(someExistingChildren isNil not and: [ someExistingChildren size > 1]) ifTrue: [ 
		^CMSignals classNodeWithMoreThanOneChildSignal raiseWith: aNode].

	anExistingChildNode :=  someExistingChildren isEmpty ifTrue: [ nil] ifFalse: [ someExistingChildren asArray first].
	anExistingChildSourceObject := nil.

	someDeltaNodes := OrderedCollection new: someExistingChildren size + 1.

	anExistingChildNode isNil ifFalse: [ 
		anExistingChildNodeAdaptor := anExistingChildNode adaptor.
		anExistingChildNodeAdaptor isNil ifFalse: [ 
			anExistingChildSourceObject := anExistingChildNodeAdaptor sourceObject.
		]
	].

	someAddedNodes 				:= OrderedCollection new: someExistingChildren size + 1.
	someRemovedNodes 			:= OrderedCollection new: someExistingChildren size + 1.

	aChildValue := self retrieveRelatedObjects.


	(anExistingChildSourceObject isNil not and: [ (anExistingChildSourceObject == aChildValue) not]) ifTrue: [ 
		aRemovedNode := aNode removeChildNodeWithObj: anExistingChildSourceObject  removedOnCollection: someDeltaNodes.
		aRemovedNode isNil ifFalse: [ someRemovedNodes add: aRemovedNode]
	].

	(aChildValue isNil not and: [ (aChildValue == anExistingChildSourceObject) not]) ifTrue: [ 
		anAddedNode := aNode addChildNodeForObj: aChildValue.
		anAddedNode isNil ifFalse: [ someAddedNodes add: anAddedNode]
	].

	(someAddedNodes isEmpty not or: [ someRemovedNodes isEmpty not]) ifTrue: [ 
		aNode changed: #children
	].
	
	"self orderChildrenValues: someChildrenValues."! !

!CMClassAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorForNode: theClass elementMap: theClassElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptorClass |
	theNode isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredClassAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorWithViewMetaInfo: theClass elementMap: theClassElementMap 
		sourceMetaInfo: theSourceMetaInfo forNode: theNode!

buildAdaptorWithViewMetaInfo: theClass elementMap: theClassElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptorClass |
	theNode isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredClassAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorWithViewMetaInfo: theClass elementMap: theClassElementMap 
		sourceMetaInfo: theSourceMetaInfo forNode: theNode!

isAdaptorFactoryForMetaInfo: theClass elementMap: theElementMap 

	theClass isNil ifTrue: [ ^false].
	theElementMap isNil ifTrue: [ ^false].

	^true! !

!CMClassChildrenNodesFactory publicMethodsFor: 'private'!

findNodeClassForObject: theObject inView: theView

	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	^self preferredObjectNodeClass!

newChildNodeForObject: theObject inNode: theNode

	| aView aNewNode aSourceMetaInfo aModelMap anElementMap someViewMetaInfos aViewMetaInfo someSubElementMaps aLikehoodEvaluator aNodeFactoryFinder aNodeFactory aClassElementMap |
	theObject isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	aView := theNode view.
	aView isNil ifTrue: [ CMSignals nodeWithoutViewSignal raiseWith: theNode].

	aClassElementMap := theNode elementMap.
	aClassElementMap isNil ifTrue: [ CMSignals nodeWithoutElementMapSignal raiseWith: theNode].

	aLikehoodEvaluator := self metaInfoLikehoodEvaluator.
	aLikehoodEvaluator isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel metaInfoLikehoodEvaluator isNil'].

	aSourceMetaInfo := self preferredViewMetaInfoUtilsClass
		findMetaInfoForSourceObject: theObject inView: aView withLikehoodEvaluator: aLikehoodEvaluator.
	aSourceMetaInfo isNil  ifTrue: [ ^CMSignals metaInfoForSourceObjectNotFoundSignal raiseWith: theObject].

	someSubElementMaps := aClassElementMap subElementMaps.
	(someSubElementMaps isNil or: [ someSubElementMaps isEmpty]) 
		ifTrue: [ 
			aModelMap := aView modelMap.
			aModelMap isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView modelMap isNil'].

			anElementMap := self preferredViewMetaInfoUtilsClass findElementMapForObject: theObject withSourceMetaInfo: aSourceMetaInfo inView: aView.

			anElementMap isNil  ifTrue: [ ^CMSignals mapElementNotFoundSignal raiseWith: theObject].

			someViewMetaInfos := anElementMap viewElements.
			(someViewMetaInfos isNil  or: [ someViewMetaInfos isEmpty]) ifTrue: [ 
				^CMSignals rootNodeViewMetaInfoForNotFoundSignal raiseWith: theObject].

			aViewMetaInfo := someViewMetaInfos first.
		]
		ifFalse: [ 
			anElementMap := someSubElementMaps detect: [:aSEM |
				aSEM sourceElementsIncludes: aSourceMetaInfo] ifNone: [ nil].
			anElementMap isNil ifTrue: [
				anElementMap := someSubElementMaps detect: [:aSEM |
					(aSEM sourceElements detect: [:aSource | aSource allSubTypes includes: aSourceMetaInfo] ifNone: [ nil]) isNil not
				] ifNone: [ nil].
			].
			anElementMap isNil ifTrue: [ ^nil].
			someViewMetaInfos := anElementMap viewElements.
			(someViewMetaInfos isNil  or: [ someViewMetaInfos isEmpty]) ifTrue: [ 
				^CMSignals collectionNodeViewMetaInfoForNotFoundSignal raiseWith: theObject].
			aViewMetaInfo := someViewMetaInfos first.
		].


	aNodeFactoryFinder := self nodeFactoryFinder.
	aNodeFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aClassChildrenNodesFactory nodeFactoryFinder isNil'].
	
	aNodeFactory := aNodeFactoryFinder findNodeFactoryForObject: theObject 
		withViewMetaInfo: aViewMetaInfo withElementMap: anElementMap withSourceMetaInfo: aSourceMetaInfo
		inView: aView.
	aNodeFactory isNil  ifTrue: [ ^CMSignals nodesFactoryNotFoundSignal raiseWith: theObject].
	
	aNewNode := aNodeFactory newNodeForObject: theObject inView: aView  sourceMetaInfo: aSourceMetaInfo elementMap: anElementMap
		viewMetaInfo: aViewMetaInfo.
	^aNewNode! !

!CMClassChildrenNodesFactory publicMethodsFor: 'svces'!

newChildrenNodesForNode: theNode

	| aViewMetaInfo anAdaptor aChildObject aNewNode |
	theNode isNil ifTrue: [ ^nil].


	aViewMetaInfo := theNode viewMetaInfo.
	aViewMetaInfo isNil ifTrue: [  ^CMSignals nodeWithoutViewMetaInfoSignal raiseWith: self].

	anAdaptor := theNode adaptor.
	anAdaptor isNil ifTrue: [  ^CMSignals nodeWithoutAdaptorSignal raiseWith: self].

	aChildObject := anAdaptor retrieveRelatedObjects.
	aChildObject isNil ifTrue: [ ^nil].

	aNewNode := self newChildNodeForObject: aChildObject inNode: theNode.

	^Array with: aNewNode! !

!CMClassNodeFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	adaptorFactoryFinder := self preferredClassAdaptorFactoryFinderClass new!

initChildrenNodesFactoryFinder
	childrenNodesFactoryFinder := self preferredClassChildrenNodesFactoryFinderClass new! !

!CMClassNodeFactory publicMethodsFor: 'private'!

findNodeClass: theClass  forNode: theNode

	theNode isNil ifTrue: [ ^nil].
	theClass isNil ifTrue: [ ^nil].

	^self preferredClassNodeClass! !

!CMClassNodeFactory publicMethodsFor: 'svces'!

isNodeFactory: theClass  forNode: theNode

	theNode isNil ifTrue: [ ^false].
	theClass isNil ifTrue: [ ^false].
	
	^true!

newInheritedNode: theClass elementMap: theElementMap forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anClassNodeClass anClassElementMap someSourceMetaInfos anClassNode aChildrenNodesFactoryFinder aChildrenNodesFactory |

	theNode isNil ifTrue: [ ^nil].
	theClass isNil ifTrue: [ ^nil].
	theElementMap isNil ifTrue: [ ^nil].

	anClassNodeClass := self findNodeClass: theClass forNode: theNode.
	anClassNodeClass isNil  ifTrue: [ ^CMSignals classNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anClassElementMap := self preferredViewMetaInfoUtilsClass scanElementMap: theElementMap forElementMapWithViewMetaInfo: theClass forNode: theNode.
	anClassElementMap isNil ifTrue: [ ^CMSignals classNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anClassElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals classNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.

	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aClassNodeFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForNode: theNode
		withViewMetaInfo: theClass withElementMap: anClassElementMap withSourceMetaInfo: aSourceMetaInfo.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theNode].
	
	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theClass elementMap: anClassElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorForNode: theClass elementMap: anClassElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anClassNode := anClassNodeClass new.

	anClassNode view: aView.
	anClassNode childrenNodesFactory: aChildrenNodesFactory.
	anClassNode sourceMetaInfo: aSourceMetaInfo.
	anClassNode elementMap: anClassElementMap.
	anClassNode viewMetaInfo: theClass.
	anClassNode adaptor: anAdaptor.

	anClassNode forzeAllAccessesPending.

	^anClassNode!

newNode: theClass  forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anClassNodeClass anClassElementMap someSourceMetaInfos anClassNode aChildrenNodesFactoryFinder aChildrenNodesFactory |

	theNode isNil ifTrue: [ ^nil].
	theClass isNil ifTrue: [ ^nil].

	anClassNodeClass := self findNodeClass: theClass forNode: theNode.
	anClassNodeClass isNil  ifTrue: [ ^CMSignals classNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anClassElementMap := self preferredViewMetaInfoUtilsClass scanNode: theNode forElementMapWithViewMetaInfo: theClass.
	anClassElementMap isNil ifTrue: [ ^CMSignals classNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anClassElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals classNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.


	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aClassNodeFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForNode: theNode
		withViewMetaInfo: theClass withElementMap: anClassElementMap withSourceMetaInfo: aSourceMetaInfo.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theNode].
	

	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theClass elementMap: anClassElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorForNode: theClass elementMap: anClassElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anClassNode := anClassNodeClass new.

	anClassNode view: aView.
	anClassNode childrenNodesFactory: aChildrenNodesFactory.
	anClassNode sourceMetaInfo: aSourceMetaInfo.
	anClassNode elementMap: anClassElementMap.
	anClassNode viewMetaInfo: theClass.
	anClassNode adaptor: anAdaptor.

	anClassNode forzeAllAccessesPending.

	^anClassNode! !

!CMCollectionAdaptor class publicMethodsFor: 'instance creation'!

newAdaptorForCollectionNode: theCollection elementMap: theCollectionElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| aCollectionAdaptor |
	aCollectionAdaptor := self new.
	aCollectionAdaptor
		initForNode: theNode  withViewMetaInfo: theCollection elementMap: theCollectionElementMap sourceMetaInfo: theSourceMetaInfo.
	^aCollectionAdaptor!

newCollectionAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo

	| anCollectionAdaptor |
	anCollectionAdaptor := self new.
	anCollectionAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anCollectionAdaptor!

newCollectionAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo
 inView: theView

	| anCollectionAdaptor |
	anCollectionAdaptor := self new.
	anCollectionAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anCollectionAdaptor! !

!CMCollectionAdaptor publicMethodsFor: 'svces'!

retrieveRelatedObjects

	| aNode aSourceObject aSourceMetaInfo aReengineeredInstVarName someObjects aParentNode aParentAdaptor |

	aNode := self node.
	aNode isNil ifTrue: [ ^nil].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [ ^nil].

	aParentAdaptor := aParentNode adaptor.
	aParentAdaptor isNil ifTrue: [ ^nil].

	aSourceObject := aParentAdaptor sourceObject.
	aSourceObject isNil ifTrue: [ ^nil].
	
	aSourceMetaInfo := aNode sourceMetaInfo.
	aSourceMetaInfo isNil ifTrue: [ ^nil].

	aReengineeredInstVarName := aSourceMetaInfo reengineredInstVarName.
	aReengineeredInstVarName isNil ifTrue: [ ^nil].

	someObjects := aSourceObject perform: aReengineeredInstVarName asSymbol.
	^someObjects! !

!CMCollectionAdaptor publicMethodsFor: 'updating'!

updateChildren

	| aNode someNodeChildrenObjects  someChildrenObjectsToAdd someChildrenObjectsToRemove someAddedNodes someRemovedNodes someChildrenValues someExistingChildren someDeltaNodes |
 
	self isReleased ifTrue: [ ^self].

	aNode := self node.
	aNode isNil ifTrue: [ ^false].

	someExistingChildren := aNode existingChildren.
	someExistingChildren isNil ifTrue: [ ^self].

	someDeltaNodes := OrderedCollection new: someExistingChildren size + 1.
	someNodeChildrenObjects := OrderedCollection new: someExistingChildren size + 1.

	someExistingChildren do: [:aChildNode |  | anAdaptor  aChildSourceObject |
		anAdaptor := aChildNode adaptor.
		anAdaptor isNil ifFalse: [ 
			aChildSourceObject := anAdaptor sourceObject.
			aChildSourceObject isNil ifFalse: [ someNodeChildrenObjects add: aChildSourceObject]
		]
	].

	someChildrenObjectsToAdd 		:= OrderedCollection new: someExistingChildren size + 1.
	someChildrenObjectsToRemove 	:= IdentitySet new: someExistingChildren size + 1.
	someAddedNodes 				:= OrderedCollection new: someExistingChildren size + 1.

	someChildrenValues := self retrieveRelatedObjects.

	someChildrenObjectsToRemove addAll: (someNodeChildrenObjects select: [:aObj | 
		(someChildrenValues detect: [:otherObj | otherObj == aObj] ifNone: [nil]) isNil]).

	someRemovedNodes := OrderedCollection new: someExistingChildren size + 1.
	someChildrenObjectsToRemove do: [:anObj |  | aRemovedNode |
		aRemovedNode := aNode removeChildNodeWithObj: anObj  removedOnCollection: someDeltaNodes.
		aRemovedNode isNil ifFalse: [ someRemovedNodes add: aRemovedNode]
	].
	


	someChildrenObjectsToAdd :=  someChildrenValues select: [:aObj | 
		(someNodeChildrenObjects detect: [:otherObj | otherObj == aObj] ifNone: [nil]) isNil].

	someChildrenObjectsToAdd do: [:anObj |  | anAddedNode |
		anAddedNode := aNode addChildNodeForObj: anObj.
		anAddedNode isNil ifFalse: [ someAddedNodes add: anAddedNode]
	].

	(someAddedNodes isEmpty not or: [ someRemovedNodes isEmpty not]) ifTrue: [ 
		aNode changed: #children
	].
	
	"self orderChildrenValues: someChildrenValues."! !

!CMCollectionAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorForNode: theCollection elementMap: theCollectionElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptorClass |
	theNode isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredCollectionAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorWithViewMetaInfo: theCollection elementMap: theCollectionElementMap 
		sourceMetaInfo: theSourceMetaInfo forNode: theNode!

buildAdaptorWithViewMetaInfo: theCollection elementMap: theCollectionElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptorClass |
	theNode isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredCollectionAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorWithViewMetaInfo: theCollection elementMap: theCollectionElementMap 
		sourceMetaInfo: theSourceMetaInfo forNode: theNode!

isAdaptorFactoryForMetaInfo: theCollection elementMap: theElementMap 

	theCollection isNil ifTrue: [ ^false].
	theElementMap isNil ifTrue: [ ^false].

	^true! !

!CMCollectionChildrenNodesFactory publicMethodsFor: 'private'!

findNodeClassForObject: theObject inView: theView

	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	^self preferredObjectNodeClass!

newChildNodeForObject: theObject inNode: theNode

	| aView aNewNode aSourceMetaInfo aModelMap anElementMap someViewMetaInfos aViewMetaInfo aCollectionElementMap someSubElementMaps aLikehoodEvaluator aNodeFactoryFinder aNodeFactory |
	theObject isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	aView := theNode view.
	aView isNil ifTrue: [ CMSignals nodeWithoutViewSignal raiseWith: theNode].

	aCollectionElementMap := theNode elementMap.
	aCollectionElementMap isNil ifTrue: [ CMSignals nodeWithoutElementMapSignal raiseWith: theNode].

	aLikehoodEvaluator := self metaInfoLikehoodEvaluator.
	aLikehoodEvaluator isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel metaInfoLikehoodEvaluator isNil'].

	aSourceMetaInfo := self preferredViewMetaInfoUtilsClass
		findMetaInfoForSourceObject: theObject inView: aView withLikehoodEvaluator: aLikehoodEvaluator.
	aSourceMetaInfo isNil  ifTrue: [ ^CMSignals metaInfoForSourceObjectNotFoundSignal raiseWith: theObject].

	someSubElementMaps := aCollectionElementMap subElementMaps.
	(someSubElementMaps isNil or: [ someSubElementMaps isEmpty]) 
		ifTrue: [ 
			aModelMap := aView modelMap.
			aModelMap isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView modelMap isNil'].

			anElementMap := self preferredViewMetaInfoUtilsClass findElementMapForObject: theObject withSourceMetaInfo: aSourceMetaInfo inView: aView.

			anElementMap isNil  ifTrue: [ ^CMSignals mapElementNotFoundSignal raiseWith: theObject].

			someViewMetaInfos := anElementMap viewElements.
			(someViewMetaInfos isNil  or: [ someViewMetaInfos isEmpty]) ifTrue: [ 
				^CMSignals rootNodeViewMetaInfoForNotFoundSignal raiseWith: theObject].

			aViewMetaInfo := someViewMetaInfos first.
		]
		ifFalse: [ 
			anElementMap := someSubElementMaps detect: [:aSEM |
				aSEM sourceElementsIncludes: aSourceMetaInfo] ifNone: [ nil].
			anElementMap isNil ifTrue: [
				anElementMap := someSubElementMaps detect: [:aSEM |
					(aSEM sourceElements detect: [:aSource | aSource allSubTypes includes: aSourceMetaInfo] ifNone: [ nil]) isNil not
				] ifNone: [ nil].
			].
			anElementMap isNil ifTrue: [ ^nil].
			someViewMetaInfos := anElementMap viewElements.
			(someViewMetaInfos isNil  or: [ someViewMetaInfos isEmpty]) ifTrue: [ 
				^CMSignals collectionNodeViewMetaInfoForNotFoundSignal raiseWith: theObject].
			aViewMetaInfo := someViewMetaInfos first.
		].


	aNodeFactoryFinder := self nodeFactoryFinder.
	aNodeFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aCollectionChildrenNodesFactory nodeFactoryFinder isNil'].
	
	aNodeFactory := aNodeFactoryFinder findNodeFactoryForObject: theObject 
		withViewMetaInfo: aViewMetaInfo withElementMap: anElementMap withSourceMetaInfo: aSourceMetaInfo
		inView: aView.
	aNodeFactory isNil  ifTrue: [ ^CMSignals nodesFactoryNotFoundSignal raiseWith: theObject].
	
	aNewNode := aNodeFactory newNodeForObject: theObject inView: aView  sourceMetaInfo: aSourceMetaInfo elementMap: anElementMap
		viewMetaInfo: aViewMetaInfo.
	^aNewNode! !

!CMCollectionChildrenNodesFactory publicMethodsFor: 'svces'!

newChildrenNodesForNode: theNode

	| aViewMetaInfo anAdaptor someChildrenObjects someNewNodes |
	theNode isNil ifTrue: [ ^nil].


	aViewMetaInfo := theNode viewMetaInfo.
	aViewMetaInfo isNil ifTrue: [  ^CMSignals nodeWithoutViewMetaInfoSignal raiseWith: self].

	anAdaptor := theNode adaptor.
	anAdaptor isNil ifTrue: [  ^CMSignals nodeWithoutAdaptorSignal raiseWith: self].

	someChildrenObjects := anAdaptor retrieveRelatedObjects.
	(someChildrenObjects isNil or: [ someChildrenObjects isEmpty]) ifTrue: [ ^nil].

	someNewNodes := OrderedCollection new: someChildrenObjects size.

	someChildrenObjects do: [:anObject |   | aNewNode |
		aNewNode := self newChildNodeForObject: anObject inNode: theNode.
		aNewNode isNil ifFalse: [ someNewNodes add: aNewNode]
	].

	^someNewNodes! !

!CMCollectionNodeFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	adaptorFactoryFinder := self preferredCollectionAdaptorFactoryFinderClass new!

initChildrenNodesFactoryFinder
	childrenNodesFactoryFinder := self preferredCollectionChildrenNodesFactoryFinderClass new! !

!CMCollectionNodeFactory publicMethodsFor: 'private'!

findNodeClass: theCollection  forNode: theNode

	theNode isNil ifTrue: [ ^nil].
	theCollection isNil ifTrue: [ ^nil].

	^self preferredCollectionNodeClass! !

!CMCollectionNodeFactory publicMethodsFor: 'svces'!

isNodeFactory: theCollection  forNode: theNode

	theNode isNil ifTrue: [ ^false].
	theCollection isNil ifTrue: [ ^false].
	
	^true!

newInheritedNode: theCollection elementMap: theElementMap forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anCollectionNodeClass anCollectionElementMap someSourceMetaInfos anCollectionNode aChildrenNodesFactoryFinder aChildrenNodesFactory |

	theNode isNil ifTrue: [ ^nil].
	theCollection isNil ifTrue: [ ^nil].
	theElementMap isNil ifTrue: [ ^nil].

	anCollectionNodeClass := self findNodeClass: theCollection forNode: theNode.
	anCollectionNodeClass isNil  ifTrue: [ ^CMSignals attributeNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anCollectionElementMap := self preferredViewMetaInfoUtilsClass scanElementMap: theElementMap forElementMapWithViewMetaInfo: theCollection forNode: theNode.
	anCollectionElementMap isNil ifTrue: [ ^CMSignals collectionNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anCollectionElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals collectionNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.
	

	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aCollectionNodeFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForNode: theNode
		withViewMetaInfo: theCollection withElementMap: anCollectionElementMap withSourceMetaInfo: aSourceMetaInfo.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theNode].
	

	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theCollection elementMap: anCollectionElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorForNode: theCollection elementMap: anCollectionElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anCollectionNode := anCollectionNodeClass new.

	anCollectionNode view: aView.
	anCollectionNode childrenNodesFactory: aChildrenNodesFactory.
	anCollectionNode sourceMetaInfo: aSourceMetaInfo.
	anCollectionNode elementMap: anCollectionElementMap.
	anCollectionNode viewMetaInfo: theCollection.
	anCollectionNode adaptor: anAdaptor.

	anCollectionNode forzeAllAccessesPending.

	^anCollectionNode!

newNode: theCollection  forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anCollectionNodeClass anCollectionElementMap someSourceMetaInfos anCollectionNode aChildrenNodesFactoryFinder aChildrenNodesFactory |

	theNode isNil ifTrue: [ ^nil].
	theCollection isNil ifTrue: [ ^nil].

	anCollectionNodeClass := self findNodeClass: theCollection forNode: theNode.
	anCollectionNodeClass isNil  ifTrue: [ ^CMSignals collectionNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anCollectionElementMap := self preferredViewMetaInfoUtilsClass scanNode: theNode forElementMapWithViewMetaInfo: theCollection.
	anCollectionElementMap isNil ifTrue: [ ^CMSignals collectionNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anCollectionElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals collectionNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.


	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aCollectionNodeFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForNode: theNode
		withViewMetaInfo: theCollection withElementMap: anCollectionElementMap withSourceMetaInfo: aSourceMetaInfo.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theNode].
	

	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theCollection elementMap: anCollectionElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorForNode: theCollection elementMap: anCollectionElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anCollectionNode := anCollectionNodeClass new.

	anCollectionNode view: aView.
	anCollectionNode childrenNodesFactory: aChildrenNodesFactory.
	anCollectionNode sourceMetaInfo: aSourceMetaInfo.
	anCollectionNode elementMap: anCollectionElementMap.
	anCollectionNode viewMetaInfo: theCollection.
	anCollectionNode adaptor: anAdaptor.

	anCollectionNode forzeAllAccessesPending.

	^anCollectionNode! !

!CMDependencyManagement class publicMethodsFor: 'instance creation'!

newForView: theView
	
	| aDependencyManagement |
	aDependencyManagement := self new.
	aDependencyManagement initForView: theView.
	^aDependencyManagement! !

!CMDependencyManagement publicMethodsFor: 'initialize-release'!

initForView: theView
	view := theView! !

!CMDependencyManagement publicMethodsFor: 'svces'!

addDependent: theNode toObject: theObject

	theObject isNil ifTrue: [ ^self].
	theNode isNil ifTrue: [ ^self].

	(theObject dependents detect: [:otherObj | otherObj == theObject] ifNone: [nil]) isNil ifTrue: [ 
		theObject addDependent: theNode
	]!

removeDependent: theNode fromObject: theObject

	theObject isNil ifTrue: [ ^self].
	theNode isNil ifTrue: [ ^self].

	[
		(theObject dependents detect: [:otherObj | otherObj == theNode] ifNone: [nil]) isNil
	] 
		whileFalse:  
	[
		theObject removeDependent: theNode
	]! !

!CMFeatureAdaptor publicMethodsFor: 'initialize-release'!

initForNode: theNode  withViewMetaInfo: theAttribute elementMap: theAttributeElementMap sourceMetaInfo: theSourceMetaInfo! !

!CMFeatureAdaptor publicMethodsFor: 'private'!

connectToObject
	
	| aNode aParentNode aParentAdaptor |
	aNode := self node.
	aNode isNil ifTrue: [ ^self].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [ ^self].

	aParentAdaptor := aParentNode adaptor.
	aParentAdaptor isNil ifTrue: [ ^self].

	aParentAdaptor addFeatureAdaptor: self! !

!CMFeatureAdaptor publicMethodsFor: 'svces'!

retrieveRelatedObjects
	self subclassResponsibility! !

!CMFeatureAdaptor publicMethodsFor: 'updating'!

shouldUpdateForAspect: theAspect

	| aNode aSourceMetaInfo aReengineeredInstVarName aShouldUpdate |
	theAspect isNil ifTrue: [ ^false].
	self isReleased ifTrue: [ ^false].

	aNode := self node.
	aNode isNil ifTrue: [ ^false].

	aSourceMetaInfo := aNode sourceMetaInfo.
	aSourceMetaInfo isNil ifTrue: [ ^false].

	aReengineeredInstVarName := aSourceMetaInfo reengineredInstVarName.
	aReengineeredInstVarName isNil ifTrue: [ ^false].

	aShouldUpdate := theAspect = aReengineeredInstVarName asSymbol.
	^aShouldUpdate!

updateChildren! !

!CMFeatureAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorForNode: theAttribute elementMap: theAttributeElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	self subclassResponsibility!

isAdaptorFactoryForMetaInfo: theOrdered elementMap: theElementMap 

	self subclassResponsibility! !

!CMFeatureNodeFactory publicMethodsFor: 'private'!

findNodeClass: theCollection  forNode: theNode
	self subclassResponsibility! !

!CMFeatureNodeFactory publicMethodsFor: 'svces'!

isNodeFactory: theAttribute  forNode: theNode

	self subclassResponsibility!

newInheritedNode: theAttribute elementMap: theElementMap forNode: theNode

	self subclassResponsibility!

newNode: theAttribute  forNode: theNode

	self subclassResponsibility! !

!CMFeatureNodeFactory publicMethodsFor: 'utils'!

scanForElementMapWithViewAttributeMetaInfo: theAttribute forNode: theNode

	| aFoundElementMap anElementMap |

	theNode isNil ifTrue: [ ^nil].
	theAttribute isNil ifTrue: [ ^nil].

	anElementMap := theNode elementMap.
	anElementMap isNil ifTrue: [  ^CMSignals nodeWithoutElementMapSignal raiseWith: self].

	aFoundElementMap :=  self scanThis: anElementMap forElementMapWithViewAttributeMetaInfo: theAttribute forNode: theNode.
	^aFoundElementMap!

scanThis: theElementMap forElementMapWithViewAttributeMetaInfo: theAttribute forNode: theNode

	| someSubElementMaps aSourceElementMap aFoundElementMap |

	theNode isNil ifTrue: [ ^nil].
	theAttribute isNil ifTrue: [ ^nil].
	theElementMap isNil ifTrue: [ ^nil].

	someSubElementMaps := theElementMap subElementMaps.
	(someSubElementMaps isNil or: [ someSubElementMaps isEmpty]) ifTrue: [ ^nil].
	
	aSourceElementMap := someSubElementMaps detect: [:anElementMap |
		anElementMap viewElementsIncludes: theAttribute] ifNone: [ nil].
	aSourceElementMap  isNil ifFalse: [ ^aSourceElementMap].

	aFoundElementMap :=  nil.
	someSubElementMaps detect: [:anElementMap |   
		aFoundElementMap := anElementMap scan: anElementMap forElementMapWithViewAttributeMetaInfo: theAttribute forNode: theNode.
		aFoundElementMap isNil not ] ifNone: [ nil].

	^aFoundElementMap! !

!CMFeatureReferencingObjectsChildrenNodesFactory publicMethodsFor: 'collaborators'!

metaInfoLikehoodEvaluator
	metaInfoLikehoodEvaluator isNil ifTrue: [ self initMetaInfoLikehoodEvaluator].
	^metaInfoLikehoodEvaluator!

nodeFactoryFinder
	nodeFactoryFinder isNil ifTrue: [ self initNodeFactoryFinder].
	^nodeFactoryFinder! !

!CMFeatureReferencingObjectsChildrenNodesFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	adaptorFactoryFinder := self preferredObjectAdaptorFactoryFinderClass new!

initAdaptorFactoryFinder: theAdaptorFactoryFinder
	adaptorFactoryFinder := theAdaptorFactoryFinder!

initMetaInfoLikehoodEvaluator
	metaInfoLikehoodEvaluator := self preferredMetaInfoLikehoodEvaluatorClass new.!

initMetaInfoLikehoodEvaluator: theMetaInfoLikehoodEvaluator
	metaInfoLikehoodEvaluator := theMetaInfoLikehoodEvaluator!

initNodeFactoryFinder
	nodeFactoryFinder := self preferredObjectNodeFactoryFinderClass new!

initNodeFactoryFinder: theNodeFactoryFinder
	nodeFactoryFinder := theNodeFactoryFinder! !

!CMFeatureReferencingObjectsChildrenNodesFactory publicMethodsFor: 'svces'!

newChildrenNodesForNode: theNode

	^Array new! !

!CMMetaInfoLikehoodEvaluator class publicMethodsFor: 'constants'!

sameAbstractClassLikehoodValue
	^100!

sameClassLikehoodValue
	^1000!

subClassLikehoodValue
	^10! !

!CMMetaInfoLikehoodEvaluator publicMethodsFor: 'private'!

scan: theType already: theScannedTypes likehood: theLikehoodHolder forSourceObject: theObject inView: theView

	| aReengineredClassName aReengineredClass aClass |
	theType isNil ifTrue: [ ^self].
	theScannedTypes isNil ifTrue: [ ^self].
	theLikehoodHolder isNil ifTrue: [ ^self].
	theObject isNil ifTrue: [ ^self].
	theView isNil ifTrue: [ ^self].

	aReengineredClassName := theType reengineredClassName.
	aReengineredClassName isNil ifTrue: [ ^self].

	aReengineredClass := Smalltalk at: aReengineredClassName ifAbsent: [ nil].
	aReengineredClass isNil ifTrue: [ ^self].

	aClass := theObject class.
	((aClass == aReengineredClass) not and: [ (aClass inheritsFrom: aReengineredClass) not]) ifTrue: [ ^self].

	aClass == aReengineredClass 
		ifTrue: [ 
			theType isAbstract 
				ifTrue: [ 
					theLikehoodHolder first < self class sameAbstractClassLikehoodValue  ifTrue: [ 
						theLikehoodHolder at: 1 put: self class sameAbstractClassLikehoodValue.
						theLikehoodHolder at: 2 put: theType.
						^self
					]
				] 
				ifFalse: [
					theLikehoodHolder first < self class sameClassLikehoodValue ifTrue: [ 
						theLikehoodHolder at: 1 put: self class sameClassLikehoodValue.
						theLikehoodHolder at: 2 put: theType.
					]
				]
		]
		ifFalse: [
			theLikehoodHolder first < self class subClassLikehoodValue ifTrue: [ 
				theLikehoodHolder at: 1 put: self class subClassLikehoodValue.
				theLikehoodHolder at: 2 put: theType.
			]	 
		].
					
	theType subTypes detect: [:aSubType |
		self scan: aSubType already: theScannedTypes likehood: theLikehoodHolder 
			forSourceObject: theObject inView: theView.
		(theLikehoodHolder at: 1) = self class sameClassLikehoodValue
	] ifNone: [ nil]! !

!CMMetaInfoLikehoodEvaluator publicMethodsFor: 'svces'!

 scanModel: theModel forSourceObject: theObject inView: theView

	| someScanRootTypes aLikehoodHolder aMostLikelyMetaInfo someScannedTypes |
	theModel isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	someScanRootTypes := theModel rootTypes.
	aLikehoodHolder := Array with: -1 with: nil.

	someScannedTypes := IdentitySet new: 37.
	someScanRootTypes do: [:aType | 
		self  scan: aType already: someScannedTypes likehood: aLikehoodHolder forSourceObject: theObject inView: theView
	].
	aMostLikelyMetaInfo := aLikehoodHolder at: 2.
	^aMostLikelyMetaInfo! !

!CMModelMap class publicMethodsFor: 'examples'!

exampleBoundCODEModelMapForCMViews
	
	"CMModelMap exampleBoundCODEModelMapForCMViews browsePath"


	| aMap |
	aMap := CODEElement newFromPersistenceAsCode: self exampleCODEModelMapForCMViews.
	aMap bindToReferencedModels.
	^aMap!

exampleModelMapForCMViews
	
	^self onMap:  (CODEElement newFromPersistenceAsCode: self exampleCODEModelMapForCMViews)! !

!CMModelMap class publicMethodsFor: 'instance creation'!

onMap: theMap
	| aModelMap |
	theMap isNil ifTrue: [ ^nil].
	aModelMap := self new.
	aModelMap map: theMap.
	^aModelMap! !

!CMModelMap class publicMethodsFor: 'modelElements persistence'!

exampleCODEModelMapForCMViews

	"(CODEElement newFromPersistenceAsCode: CMModelMap exampleCODEModelMapForCMViews) browsePath"

	self ojoModel.

	^   #( map 'ExampleModelMapForCMViews'
	nil nil
	nil
	nil
	CMModelMap exampleCODEModelMapForCMViews
	nil
	(submodules
	  ( mapsFolder 'Core'
		nil nil
		nil
		nil
		nil nil
		nil
		nil
		(elementMaps
		  ( elementMap 'Element'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			(subElementMaps
			  ( elementMap 'identifier'
				nil nil
				nil
				nil
				( mapLogic 'ATTRIBUTE'
				  nil nil
				  nil
				  nil
				  ATTRIBUTE
				 )

				nil
				nil
				nil
			   )

			 )
			nil
			nil
		   )

		  ( elementMap 'Package'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			(subElementMaps
			  ( elementMap 'subPackages'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'superPackage'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'objects'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			 )
			nil
			nil
		   )

		  ( elementMap 'Specification'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			nil
			nil
			nil
		   )

		  ( elementMap 'Property'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			(subElementMaps
			  ( elementMap 'object'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'valueObject'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			 )
			nil
			nil
		   )

		  ( elementMap 'Link'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			(subElementMaps
			  ( elementMap 'object'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'relatedObject'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			 )
			nil
			nil
		   )

		  ( elementMap 'Object'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			(subElementMaps
			  ( elementMap 'instantiable'
				nil nil
				nil
				nil
				( mapLogic 'ATTRIBUTE'
				  nil nil
				  nil
				  nil
				  ATTRIBUTE
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'package'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'links'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'properties'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'superObjects'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'subObjects'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'linkedWith'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			  ( elementMap 'objectOfProperties'
				nil nil
				nil
				nil
				( mapLogic 'RELATIONSHIP'
				  nil nil
				  nil
				  nil
				  RELATIONSHIP
				 )

				nil
				nil
				nil
			   )

			 )
			nil
			nil
		   )

		 )
	   )

	  ( mapsFolder 'DataTypes'
		nil nil
		nil
		nil
		nil nil
		nil
		nil
		(elementMaps
		  ( elementMap 'String'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			nil
			nil
			nil
		   )

		  ( elementMap 'Symbol'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			nil
			nil
			nil
		   )

		  ( elementMap 'Boolean'
			nil nil
			nil
			nil
			( mapLogic 'TYPE'
			  nil nil
			  nil
			  nil
			  TYPE
			 )

			nil
			nil
			nil
		   )

		 )
	   )

	 )
	nil nil
	nil
	nil
	nil
   )! !

!CMModelMap class publicMethodsFor: 'navigation'!

modelEditorMETAPerspectives

	^self modelEditorMETAPerspectivesModelMap, self modelEditorMETAPerspectivesGeneral, 
		super modelEditorMETAPerspectives!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Name')));
		yourself!

modelEditorMETAPerspectivesModelMap
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Map'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Map')));
		yourself!

modelEditorMETASelectors

	^super modelEditorMETASelectors , self modelEditorMETASelectorsModelMap!

modelEditorMETASelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMView selector: #modelEditorMETASelectorsGeneral target: nil selector: nil."

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
		yourself!

modelEditorMETASelectorsModelMap

	"METAChildSpecAutoViewEditor openOn: CMModelMap selector: #modelEditorMETASelectorsModelMap target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 3)
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Map';
			basicSelector: #map;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Map';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEModel;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		yourself!

modelEditorPathSelectors

	| aLocalView |
	aLocalView := self modelEditorPathSelectorsModelMap.

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: aLocalView metaSelectorsSelector;
		pathSelectorsSelector: aLocalView pathSelectorsSelector;
		metaSelectorsToSelect: aLocalView metaSelectorsToSelect, super modelEditorPathSelectors metaSelectorsToSelect!

modelEditorPathSelectorsModelMap

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #( 'Map')! !

!CMModelMap class publicMethodsFor: 'ojo'!

ojoMap!

ojoModel! !

!CMModelMap publicMethodsFor: 'accessing'!

map
	^map!

map: theMap
	map := theMap!

name
	^self map isNil ifTrue: ['unknown map' copy] ifFalse: [ self map name]! !

!CMModelMap publicMethodsFor: 'navigation'!

displayName
	^self name!

metaNameSelector
	^#name! !

!CMNodeFactory publicMethodsFor: 'collaborators'!

adaptorFactoryFinder
	adaptorFactoryFinder isNil ifTrue: [ self initAdaptorFactoryFinder].
	^adaptorFactoryFinder!

childrenNodesFactoryFinder
	childrenNodesFactoryFinder isNil ifTrue: [ self initChildrenNodesFactoryFinder].
	^childrenNodesFactoryFinder! !

!CMNodeFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	self subclassResponsibility!

initAdaptorFactoryFinder: theAdaptorFactoryFinder
	adaptorFactoryFinder := theAdaptorFactoryFinder!

initChildrenNodesFactoryFinder
	self subclassResponsibility!

initChildrenNodesFactoryFinder: theChildrenNodesFactoryFinder
	childrenNodesFactoryFinder := theChildrenNodesFactoryFinder! !

!CMObjectAdaptor class publicMethodsFor: 'instance creation'!

newAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo

	| anAdaptor |
	anAdaptor := self new.
	anAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anAdaptor!

newAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo
 inView: theView

	| anAdaptor |
	anAdaptor := self new.
	anAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anAdaptor! !

!CMObjectAdaptor publicMethodsFor: 'initialize-release'!

initForNode: theNode  withViewMetaInfo: theAttribute elementMap: theAttributeElementMap sourceMetaInfo: theSourceMetaInfo

	node := theNode.

	theNode isNil ifTrue: [ ^self].! !

!CMObjectAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo inView: theView

	| anAdaptorClass |
	theObject isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredObjectAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap 
		sourceMetaInfo: theSourceMetaInfo inView: theView!

isAdaptorFactoryForObject: theObject inView: theView

	theObject isNil ifFalse: [ ^false].
	theView isNil ifFalse: [ ^false].

	^true! !

!CMObjectChildrenNodesFactory publicMethodsFor: 'initialize-collaborators'!

initAttributeNodeFactoryFinder
	attributeNodeFactoryFinder := self preferredAttributeNodeFactoryFinderClass new!

initClassNodeFactoryFinder
	classNodeFactoryFinder := self preferredClassNodeFactoryFinderClass new!

initCollectionNodeFactoryFinder
	collectionNodeFactoryFinder := self preferredCollectionNodeFactoryFinderClass new!

initOrderedNodeFactoryFinder
	orderedNodeFactoryFinder := self preferredOrderedNodeFactoryFinderClass new! !

!CMObjectNodeFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	adaptorFactoryFinder := self preferredObjectAdaptorFactoryFinderClass new!

initChildrenNodesFactoryFinder
	childrenNodesFactoryFinder := self preferredObjectChildrenNodesFactoryFinderClass new!

initMetaInfoLikehoodEvaluator
	metaInfoLikehoodEvaluator := self preferredMetaInfoLikehoodEvaluatorClass new.! !

!CMObjectNodeFactory publicMethodsFor: 'private'!

findNodeClassForObject: theObject inView: theView

	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	^self preferredObjectNodeClass! !

!CMObjectNodeFactory publicMethodsFor: 'svces'!

isNodeFactoryForObject: theObject inView: theView

	theObject isNil ifFalse: [ ^false].
	theView isNil ifFalse: [ ^false].

	^true!

newNodeForObject: theObject inView: theView  sourceMetaInfo: theSourceMetaInfo elementMap: theElementMap
	viewMetaInfo: theViewMetaInfo 

	| aNewNode aChildrenNodesFactoryFinder aChildrenNodesFactory anAdaptorFactoryFinder anAdaptorFactory anAdaptor aNewNodeClass |

	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].
	theSourceMetaInfo isNil ifTrue: [ ^nil].
	theElementMap isNil ifTrue: [ ^nil].
	theViewMetaInfo isNil ifTrue: [ ^nil].

	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForObject: theObject 
		withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap withSourceMetaInfo: theSourceMetaInfo
		inView: theView.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theObject].
	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder findAdaptorFactoryForObject: theObject 
		withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap withSourceMetaInfo: theSourceMetaInfo
		inView: theView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theObject].
	  
	anAdaptor := anAdaptorFactory buildAdaptorForObject: theObject 
		withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap withSourceMetaInfo: theSourceMetaInfo
		inView: theView.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theObject].


	aNewNodeClass := self findNodeClassForObject: theObject inView: theView.
	aNewNodeClass isNil  ifTrue: [ ^CMSignals rootNodesClassNotFoundSignal raiseWith: theObject].
	
	aNewNode := aNewNodeClass new.

	aNewNode view: 						theView.
	aNewNode childrenNodesFactory: 	aChildrenNodesFactory.
	aNewNode sourceMetaInfo: 			theSourceMetaInfo.
	aNewNode elementMap: 				theElementMap.
	aNewNode viewMetaInfo: 				theViewMetaInfo.
	aNewNode adaptor: 					anAdaptor.

	aNewNode forzeAllAccessesPending.

	^aNewNode! !

!CMOrderedAdaptor class publicMethodsFor: 'instance creation'!

newAdaptorForOrderedNode: theOrdered elementMap: theOrderedElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anOrderedAdaptor |
	anOrderedAdaptor := self new.
	anOrderedAdaptor
		initForNode: theNode  withViewMetaInfo: theOrdered elementMap: theOrderedElementMap sourceMetaInfo: theSourceMetaInfo.
	^anOrderedAdaptor!

newOrderedAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo

	| anOrderedAdaptor |
	anOrderedAdaptor := self new.
	anOrderedAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anOrderedAdaptor!

newOrderedAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo
 inView: theView

	| anOrderedAdaptor |
	anOrderedAdaptor := self new.
	anOrderedAdaptor
		initForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap sourceMetaInfo: theSourceMetaInfo.
	^anOrderedAdaptor! !

!CMOrderedAdaptor publicMethodsFor: 'svces'!

retrieveRelatedObjects

	| aNode aSourceObject aSourceMetaInfo aReengineeredInstVarName someObjects aParentNode aParentAdaptor |
	aNode := self node.
	aNode isNil ifTrue: [ ^nil].

	aParentNode := aNode parent.
	aParentNode isNil ifTrue: [ ^nil].

	aParentAdaptor := aParentNode adaptor.
	aParentAdaptor isNil ifTrue: [ ^nil].

	aSourceObject := aParentAdaptor sourceObject.
	aSourceObject isNil ifTrue: [ ^nil].
	
	aSourceMetaInfo := aNode sourceMetaInfo.
	aSourceMetaInfo isNil ifTrue: [ ^nil].

	aReengineeredInstVarName := aSourceMetaInfo reengineredInstVarName.
	aReengineeredInstVarName isNil ifTrue: [ ^nil].

	someObjects := aSourceObject perform: aReengineeredInstVarName.
	^someObjects! !

!CMOrderedAdaptor publicMethodsFor: 'updating'!

updateChildren

	| aNode someNodeChildrenObjects  someChildrenObjectsToAdd someChildrenObjectsToRemove someAddedNodes someRemovedNodes someChildrenValues someExistingChildren someDeltaNodes |
 
	self isReleased ifTrue: [ ^self].

	aNode := self node.
	aNode isNil ifTrue: [ ^false].

	someExistingChildren := aNode existingChildren.
	someExistingChildren isNil ifTrue: [ ^self].

	someDeltaNodes := OrderedCollection new: someExistingChildren size + 1.
	someNodeChildrenObjects := OrderedCollection new: someExistingChildren size + 1.

	someExistingChildren do: [:aChildNode |  | anAdaptor  aChildSourceObject |
		anAdaptor := aChildNode adaptor.
		anAdaptor isNil ifFalse: [ 
			aChildSourceObject := anAdaptor sourceObject.
			aChildSourceObject isNil ifFalse: [ someNodeChildrenObjects add: aChildSourceObject]
		]
	].

	someChildrenObjectsToAdd 		:= OrderedCollection new: someExistingChildren size + 1.
	someChildrenObjectsToRemove 	:= IdentitySet new: someExistingChildren size + 1.
	someAddedNodes 				:= OrderedCollection new: someExistingChildren size + 1.

	someChildrenValues := self retrieveRelatedObjects.

	someChildrenObjectsToRemove addAll: (someNodeChildrenObjects select: [:aObj | 
		(someChildrenValues detect: [:otherObj | otherObj == aObj] ifNone: [nil]) isNil]).

	someRemovedNodes := OrderedCollection new: someExistingChildren size + 1.
	someChildrenObjectsToRemove do: [:anObj |  | aRemovedNode |
		aRemovedNode := aNode removeChildNodeWithObj: anObj  removedOnCollection: someDeltaNodes.
		aRemovedNode isNil ifFalse: [ someRemovedNodes add: aRemovedNode]
	].
	


	someChildrenObjectsToAdd :=  someChildrenValues select: [:aObj | 
		(someNodeChildrenObjects detect: [:otherObj | otherObj == aObj] ifNone: [nil]) isNil].

	someChildrenObjectsToAdd do: [:anObj |  | anAddedNode |
		anAddedNode := aNode addChildNodeForObj: anObj.
		anAddedNode isNil ifFalse: [ someAddedNodes add: anAddedNode]
	].

	(someAddedNodes isEmpty not or: [ someRemovedNodes isEmpty not]) ifTrue: [ 
		aNode changed: #children
	].
	
	"self orderChildrenValues: someChildrenValues."! !

!CMOrderedAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorForNode: theOrdered elementMap: theOrderedElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptorClass |
	theNode isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredOrderedAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorWithViewMetaInfo: theOrdered elementMap: theOrderedElementMap 
		sourceMetaInfo: theSourceMetaInfo forNode: theNode!

buildAdaptorWithViewMetaInfo: theOrdered elementMap: theOrderedElementMap 
	sourceMetaInfo: theSourceMetaInfo forNode: theNode

	| anAdaptorClass |
	theNode isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredOrderedAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorWithViewMetaInfo: theOrdered elementMap: theOrderedElementMap 
		sourceMetaInfo: theSourceMetaInfo forNode: theNode!

isAdaptorFactoryForMetaInfo: theOrdered elementMap: theElementMap 

	theOrdered isNil ifTrue: [ ^false].
	theElementMap isNil ifTrue: [ ^false].

	^true! !

!CMOrderedNodeFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	adaptorFactoryFinder := self preferredOrderedAdaptorFactoryFinderClass new!

initChildrenNodesFactoryFinder
	childrenNodesFactoryFinder := self preferredOrderedChildrenNodesFactoryFinderClass new! !

!CMOrderedNodeFactory publicMethodsFor: 'private'!

findNodeClass: theOrdered  forNode: theNode

	theNode isNil ifTrue: [ ^nil].
	theOrdered isNil ifTrue: [ ^nil].

	^self preferredOrderedNodeClass! !

!CMOrderedNodeFactory publicMethodsFor: 'svces'!

isNodeFactory: theOrdered  forNode: theNode

	theNode isNil ifTrue: [ ^false].
	theOrdered isNil ifTrue: [ ^false].
	
	^true!

newInheritedNode: theOrdered elementMap: theElementMap forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anOrderedNodeClass anOrderedElementMap someSourceMetaInfos anOrderedNode aChildrenNodesFactoryFinder aChildrenNodesFactory |

	theNode isNil ifTrue: [ ^nil].
	theOrdered isNil ifTrue: [ ^nil].
	theElementMap isNil ifTrue: [ ^nil].

	anOrderedNodeClass := self findNodeClass: theOrdered forNode: theNode.
	anOrderedNodeClass isNil  ifTrue: [ ^CMSignals orderedNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anOrderedElementMap := self preferredViewMetaInfoUtilsClass scanElementMap: theElementMap forElementMapWithViewMetaInfo: theOrdered forNode: theNode.
	anOrderedElementMap isNil ifTrue: [ ^CMSignals orderedNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anOrderedElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals orderedNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.
	
	
	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aCollectionNodeFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForNode: theNode
		withViewMetaInfo: theOrdered withElementMap: anOrderedElementMap withSourceMetaInfo: aSourceMetaInfo.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theOrdered elementMap: anOrderedElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorForNode: theOrdered elementMap: anOrderedElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anOrderedNode := anOrderedNodeClass new.

	anOrderedNode view: aView.
	anOrderedNode childrenNodesFactory: aChildrenNodesFactory.
	anOrderedNode sourceMetaInfo: aSourceMetaInfo.
	anOrderedNode elementMap: anOrderedElementMap.
	anOrderedNode viewMetaInfo: theOrdered.
	anOrderedNode adaptor: anAdaptor.

	anOrderedNode forzeAllAccessesPending.

	^anOrderedNode!

newNode: theOrdered  forNode: theNode
  
| aView aSourceMetaInfo anAdaptor anAdaptorFactoryFinder anAdaptorFactory anOrderedNodeClass anOrderedElementMap someSourceMetaInfos anOrderedNode aChildrenNodesFactoryFinder aChildrenNodesFactory |

	theNode isNil ifTrue: [ ^nil].
	theOrdered isNil ifTrue: [ ^nil].

	anOrderedNodeClass := self findNodeClass: theOrdered forNode: theNode.
	anOrderedNodeClass isNil  ifTrue: [ ^CMSignals orderedNodeClassNotFoundSignal raiseWith: theNode].
	
	aView := theNode view.
	aView isNil ifTrue: [ ^CMSignals nodeWithoutViewSignal raiseWith: theNode].

	anOrderedElementMap := self preferredViewMetaInfoUtilsClass scanNode: theNode forElementMapWithViewMetaInfo: theOrdered.
	anOrderedElementMap isNil ifTrue: [ ^CMSignals orderedNodeElementMapNotFoundSignal raiseWith: theNode].

	someSourceMetaInfos := anOrderedElementMap sourceElements.
	(someSourceMetaInfos isNil  or: [ someSourceMetaInfos isEmpty]) ifTrue: [
		^CMSignals orderedNodeSourceMetaInfoNotFoundSignal raiseWith: theNode].

	aSourceMetaInfo := someSourceMetaInfos first.
	
	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aCollectionNodeFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForNode: theNode
		withViewMetaInfo: theOrdered withElementMap: anOrderedElementMap withSourceMetaInfo: aSourceMetaInfo.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder  findAdaptorFactoryWithViewMetaInfo: theOrdered elementMap: anOrderedElementMap inView: aView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theNode].
	
	anAdaptor := anAdaptorFactory buildAdaptorForNode: theOrdered elementMap: anOrderedElementMap 
		sourceMetaInfo: aSourceMetaInfo forNode: theNode.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theNode].

	anOrderedNode := anOrderedNodeClass new.

	anOrderedNode view: aView.
	anOrderedNode childrenNodesFactory: aChildrenNodesFactory.
	anOrderedNode sourceMetaInfo: aSourceMetaInfo.
	anOrderedNode elementMap: anOrderedElementMap.
	anOrderedNode viewMetaInfo: theOrdered.
	anOrderedNode adaptor: anAdaptor.

	anOrderedNode forzeAllAccessesPending.

	^anOrderedNode! !

!CMRootAdaptorFactory publicMethodsFor: 'svces'!

buildAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo withElementMap: theElementMap 
	withSourceMetaInfo: theSourceMetaInfo inView: theView

	| anAdaptorClass |
	theObject isNil ifTrue: [ ^nil].

	anAdaptorClass := self preferredRootAdaptorClass.
	anAdaptorClass isNil ifTrue: [ ^nil].
	 
	^anAdaptorClass newAdaptorForObject: theObject  withViewMetaInfo: theViewMetaInfo elementMap: theElementMap 
		sourceMetaInfo: theSourceMetaInfo inView: theView!

isAdaptorFactoryForObject: theObject inView: theView

	theObject isNil ifFalse: [ ^false].
	theView isNil ifFalse: [ ^false].

	^true! !

!CMRootChildrenNodesFactory publicMethodsFor: 'initialize-collaborators'!

initAttributeNodeFactoryFinder
	attributeNodeFactoryFinder := self preferredAttributeNodeFactoryFinderClass new!

initClassNodeFactoryFinder
	classNodeFactoryFinder := self preferredClassNodeFactoryFinderClass new!

initCollectionNodeFactoryFinder
	collectionNodeFactoryFinder := self preferredCollectionNodeFactoryFinderClass new!

initOrderedNodeFactoryFinder
	orderedNodeFactoryFinder := self preferredOrderedNodeFactoryFinderClass new! !

!CMRootNodeFactory publicMethodsFor: 'initialize-collaborators'!

initAdaptorFactoryFinder
	adaptorFactoryFinder := self preferredRootAdaptorFactoryFinderClass new!

initChildrenNodesFactoryFinder
	childrenNodesFactoryFinder := self preferredRootChildrenNodesFactoryFinderClass new!

initMetaInfoLikehoodEvaluator
	metaInfoLikehoodEvaluator := self preferredMetaInfoLikehoodEvaluatorClass new.! !

!CMRootNodeFactory publicMethodsFor: 'private'!

findNodeClassForObject: theObject inView: theView

	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	^self preferredRootNodeClass! !

!CMRootNodeFactory publicMethodsFor: 'svces'!

isNodeFactoryForObject: theObject inView: theView

	theObject isNil ifFalse: [ ^false].
	theView isNil ifFalse: [ ^false].

	^true!

newNodeForObject: theObject inView: theView

	| aRootNodeClass aRootNode aSourceMetaInfo aViewMetaInfo aChildrenNodesFactoryFinder aChildrenNodesFactory anElementMap someViewMetaInfos anAdaptor anAdaptorFactoryFinder anAdaptorFactory aModelMap aLikehoodEvaluator |

	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	aRootNodeClass := self findNodeClassForObject: theObject inView: theView.
	aRootNodeClass isNil  ifTrue: [ ^CMSignals rootNodesClassNotFoundSignal raiseWith: theObject].
	
	aRootNode := aRootNodeClass new.

	aLikehoodEvaluator := self metaInfoLikehoodEvaluator.
	aLikehoodEvaluator isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel metaInfoLikehoodEvaluator isNil'].

	aSourceMetaInfo := self preferredViewMetaInfoUtilsClass
		findMetaInfoForSourceObject: theObject inView: theView withLikehoodEvaluator: aLikehoodEvaluator.
	aSourceMetaInfo isNil  ifTrue: [ ^CMSignals metaInfoForSourceObjectNotFoundSignal raiseWith: theObject].
	
	aModelMap := theView modelMap.
	aModelMap isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView modelMap isNil'].

	anElementMap := self preferredViewMetaInfoUtilsClass findElementMapForObject: theObject withSourceMetaInfo: aSourceMetaInfo inView: theView.

	anElementMap isNil  ifTrue: [ ^CMSignals mapElementNotFoundSignal raiseWith: theObject].

	someViewMetaInfos := anElementMap viewElements.
	(someViewMetaInfos isNil  or: [ someViewMetaInfos isEmpty]) ifTrue: [ ^CMSignals rootNodeViewMetaInfoForNotFoundSignal raiseWith: theObject].

	aViewMetaInfo := someViewMetaInfos first.

	aChildrenNodesFactoryFinder := self childrenNodesFactoryFinder.
	aChildrenNodesFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory childrenNodesFactoryFinder isNil'].
	
	aChildrenNodesFactory := aChildrenNodesFactoryFinder findChildrenNodesFactoryForObject: theObject 
		withViewMetaInfo: aViewMetaInfo withElementMap: anElementMap withSourceMetaInfo: aSourceMetaInfo
		inView: theView.
	aChildrenNodesFactory isNil  ifTrue: [ ^CMSignals childrenNodesFactoryNotFoundSignal raiseWith: theObject].
	
	anAdaptorFactoryFinder := self adaptorFactoryFinder.
	anAdaptorFactoryFinder isNil ifTrue: [
		 ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesFactory adaptorFactoryFinder isNil'].

	anAdaptorFactory := anAdaptorFactoryFinder findAdaptorFactoryForObject: theObject 
		withViewMetaInfo: aViewMetaInfo withElementMap: anElementMap withSourceMetaInfo: aSourceMetaInfo
		inView: theView.
	anAdaptorFactory isNil  ifTrue: [ ^CMSignals adaptorFactoryNotFoundSignal raiseWith: theObject].
	  
	anAdaptor := anAdaptorFactory buildAdaptorForObject: theObject 
		withViewMetaInfo: aViewMetaInfo withElementMap: anElementMap withSourceMetaInfo: aSourceMetaInfo
		inView: theView.
	anAdaptor isNil  ifTrue: [ ^CMSignals adaptorNotBuiltSignal raiseWith: theObject].

	aRootNode view: 						theView.
	aRootNode childrenNodesFactory: 	aChildrenNodesFactory.
	aRootNode sourceMetaInfo: 			aSourceMetaInfo.
	aRootNode elementMap: 				anElementMap.
	aRootNode viewMetaInfo: 				aViewMetaInfo.
	aRootNode adaptor: 					anAdaptor.

	aRootNode forzeAllAccessesPending.
	aRootNode connectAdaptor.

	^aRootNode! !

!CMRootNodesManager class publicMethodsFor: 'instance creation'!

newForView: theView
	
	| aRootNodesManager |
	aRootNodesManager := self new.
	aRootNodesManager initForView: theView.
	^aRootNodesManager! !

!CMRootNodesManager class publicMethodsFor: 'navigation'!

modelEditorMETAPerspectives

	^self modelEditorMETAPerspectivesRootNodesManager, 
		super modelEditorMETAPerspectives!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Name')));
		yourself!

modelEditorMETAPerspectivesRootNodesManager
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'RootNodes'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'RootNodes')));
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'View'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'View')));
		yourself!

modelEditorMETASelectors

	^super modelEditorMETASelectors , self modelEditorMETASelectorsRootNodesManager!

modelEditorMETASelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMView selector: #modelEditorMETASelectorsGeneral target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 5)
		yourself!

modelEditorMETASelectorsRootNodesManager

	"METAChildSpecAutoViewEditor openOn: CMRootNodesManager selector: #modelEditorMETASelectorsRootNodesManager target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 2)
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RootNodes';
			basicSelector: #rootNodes;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RootNodes';
			displaySelector: #descriptionString;
			canShowInTree: true;
			componentsClassName: #CMAbstractNode;
			sortSelector: nil;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			inheritanceLinkSelector: nil;
			menuSelector: nil;
			autoFilter: true;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'View';
			basicSelector: #view;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'View';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CMView;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		yourself!

modelEditorPathSelectors

	| aLocalView |
	aLocalView := self modelEditorPathSelectorsRootNodesManager.

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: aLocalView metaSelectorsSelector;
		pathSelectorsSelector: aLocalView pathSelectorsSelector;
		metaSelectorsToSelect: aLocalView metaSelectorsToSelect, super modelEditorPathSelectors metaSelectorsToSelect!

modelEditorPathSelectorsRootNodesManager

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #( 'RootNodes'  'View')! !

!CMRootNodesManager publicMethodsFor: 'accessing'!

rootNodes
	rootNodes isNil ifTrue: [ self initRootNodes].
	^rootNodes!

view
	^view! !

!CMRootNodesManager publicMethodsFor: 'collaborators'!

rootNodesFactoryFinder
	rootNodesFactoryFinder isNil ifTrue: [ self initRootNodesFactoryFinder].
	^rootNodesFactoryFinder! !

!CMRootNodesManager publicMethodsFor: 'initialize-collaborators'!

initRootNodesFactoryFinder
	rootNodesFactoryFinder := self preferredRootNodeFactoryFinderClass new!

initRootNodesFactoryFinder: theRootNodesFactoryFinder
	rootNodesFactoryFinder := theRootNodesFactoryFinder! !

!CMRootNodesManager publicMethodsFor: 'initialize-release'!

initForView: theView
	view := theView!

initRootNodes
	rootNodes := OrderedCollection new: 4!

release

	self releaseRootNodes.

	view := nil.
	rootNodes := nil.
	rootNodesFactoryFinder := nil.
	super release!

releaseRootNodes

	rootNodes isNil ifFalse: [ 
		rootNodes copy do: [:aNode | 
			aNode recursiveRelease.
			rootNodes remove: aNode
		]
	].! !

!CMRootNodesManager publicMethodsFor: 'notifying'!

notifyRootNodesChange
	self changed: #rootNodes.! !

!CMRootNodesManager publicMethodsFor: 'svces'!

existingRootNodeForObject: theObject

	| someRoots anExistingRoot |

	theObject isNil ifTrue: [ ^nil].

	someRoots := self rootNodes.
	anExistingRoot := someRoots detect: [:aRootNode | aRootNode isRootNodeForObject: theObject] ifNone: [ nil].

	^anExistingRoot!

newOrExistingRootNodeForObject: theObject

	| aNewNode  aRootNode |

	theObject isNil ifTrue: [ ^nil].

	aRootNode := self existingRootNodeForObject: theObject.
	aRootNode isNil ifFalse: [ ^aRootNode].

	aNewNode := self newRootNodeForObject: theObject.
	^aNewNode!

newRootNodeForObject: theObject

	| aView aNewNode someRoots aFactoryFinder aFactory |
	theObject isNil ifTrue: [ ^nil].

	aView := self view.
	aView isNil ifTrue: [ ^nil].
	
	aFactoryFinder := self rootNodesFactoryFinder.
	aFactoryFinder isNil ifTrue: [ ^CMSignals rootNodesFactoryFinderNotFoundSignal raise].
	
	aFactory := aFactoryFinder findNodeFactoryForObject: theObject inView: aView.
	aFactory isNil ifTrue: [ ^CMSignals rootNodesFactoryNotFoundSignal raiseWith: theObject].

	aNewNode := aFactory newNodeForObject: theObject inView: aView.
	aNewNode isNil ifTrue: [ ^CMSignals newRootNodeFailed raiseWith: theObject].
	
	someRoots := self rootNodes.
	someRoots isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aRootNodesManager rootNodes isNil'].

	someRoots add: aNewNode.

	self notifyRootNodesChange.

	
	^aNewNode! !

!CMSignals class publicMethodsFor: 'class initialization'!

initialize
	"CMSignals initialize"

	self initSignals!

initSignals
	"CMSignals initSignals"

	CMSignal :=  (Signal genericSignal newSignalMayProceed: true) 
			notifierString: 'CM signal';
			nameClass: self 
			message: #cmSignal.
	CMQueryRecursionSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Query for recursion';
			nameClass: self 
			message: #cmQueryRecursionSignal.
	CMViewBadSetupSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'CMView is not properly setup';
			nameClass: self 
			message: #cmViewBadSetupSignal.

	MetaInfoForRootNodeNotFoundSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not find metaInfoForRootNode';
			nameClass: self 
			message: #metaInfoForRootNodeNotFoundSignal.

	MetaInfoForSourceObjectNotFoundSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not find metaInfoForSourceObject';
			nameClass: self 
			message: #metaInfoForSourceObjectNotFoundSignal.

	NewRootNodeFailed := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not create new RootNode';
			nameClass: self 
			message: #newRootNodeFailed.

	RootNodesFactoryFinderNotFoundSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not find rootNodesFactoryFinder';
			nameClass: self 
			message: #rootNodesFactoryFinderNotFoundSignal.

	RootNodesFactoryNotFoundSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not find rootNodesFactory';
			nameClass: self 
			message: #rootNodesFactoryNotFoundSignal.

	RootNodesFactoryClassNotFoundSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not find rootNodesFactoryClass';
			nameClass: self 
			message: #rootNodesFactoryClassNotFoundSignal.

	ModelAccessorNotFoundSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not find modelAccessor';
			nameClass: self 
			message: #modelAccessorNotFoundSignal.

	MapInfoNotFoundSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Could not find mapInfo';
			nameClass: self 
			message: #mapInfoNotFoundSignal.

	NodeWithoutChildrenNodesFactorySignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Node does not have ChildrenNodesFactory';
			nameClass: self 
			message: #nodeWithoutChildrenNodesFactorySignal.

	NodeWithoutAdaptorSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Node does not have Adaptor';
			nameClass: self 
			message: #nodeWithoutAdaptorSignal.

	NodeWithoutViewMetaInfoSignal := 	(CMSignals cmSignal newSignalMayProceed: false) 
			notifierString: 'Node does not have ViewMetaInfo';
			nameClass: self 
			message: #nodeWithoutViewMetaInfoSignal.! !

!CMSignals class publicMethodsFor: 'signals'!

childrenNodesFactoryNotFoundSignal
	^ChildrenNodesFactoryNotFoundSignal!

cmQueryRecursionSignal
	^CMQueryRecursionSignal!

cmSignal
	^CMSignal!

cmViewBadSetupSignal
	^CMViewBadSetupSignal!

mapInfoNotFoundSignal
	^MapInfoNotFoundSignal!

metaInfoForRootNodeNotFoundSignal
	^MetaInfoForRootNodeNotFoundSignal!

metaInfoForSourceObjectNotFoundSignal
	^MetaInfoForSourceObjectNotFoundSignal!

modelAccessorNotFoundSignal
	^ModelAccessorNotFoundSignal!

newRootNodeFailed
	^NewRootNodeFailed!

nodeWithoutAccessorSignal
	^NodeWithoutAccessorSignal!

nodeWithoutChildrenFactory
	^NodeWithoutChildrenFactory!

nodeWithoutChildrenNodesFactorySignal
	^NodeWithoutChildrenNodesFactorySignal!

nodeWithoutViewMetaInfoSignal
	^NodeWithoutViewMetaInfoSignal!

rootNodesFactoryClassNotFoundSignal
	^RootNodesFactoryClassNotFoundSignal!

rootNodesFactoryFinderNotFoundSignal
	^RootNodesFactoryFinderNotFoundSignal!

rootNodesFactoryNotFoundSignal
	^RootNodesFactoryNotFoundSignal! !

!CMSourceModel class publicMethodsFor: 'examples'!

exampleSourceModelForCMViews

	^self onModel: (CODEElement newFromPersistenceAsCode: self exampleCODESourceModelForCMViews)!

exampleSourceModelInstanceForCMViews
	^CODEElement newFromPersistenceAsCode: PROTOprENV13606MetaInfoHolder prENV13606Store! !

!CMSourceModel class publicMethodsFor: 'instance creation'!

onModel: theModel
	| aSourceModel |
	theModel isNil ifTrue: [ ^nil].
	aSourceModel := self new.
	aSourceModel model: theModel.
	^aSourceModel! !

!CMSourceModel class publicMethodsFor: 'modelElements persistence'!

exampleCODESourceModelForCMViews

	"(CODEElement newFromPersistenceAsCode: CMSourceModel exampleCODESourceModelForCMViews) browsePath"

	self ojoModel.

	^   #( model 'ExampleSourceModelForCMViews'
	nil nil
	nil
	nil
	CMSourceModel exampleCODESourceModelForCMViews
	nil
	(submodules
	  ( module 'Core'
		nil nil
		(definedAspects
		  ( aspect 'a'
			nil nil
		   )

		 )
		nil
		nil nil
		(types
		  ( type 'Element'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEElement
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Comment'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEComment
			false false false false
			#(#supertypes #(#refToType 'Element' 'Core'))
			nil
			(attributes
			  ( relationship 'element'
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
				( refToInverseRelationship 'comments'  ( refToType 'CommentedElement' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'element'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'comments'  ( refToType 'CommentedElement' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'CommentedElement'
			nil nil
			nil
			nil
			true false false
			nil nil
			CODECommentedElement
			false false false false
			#(#supertypes #(#refToType 'Element' 'Core'))
			nil
			(attributes
			  ( relationship 'comments'
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
				( refToInverseRelationship 'element'  ( refToType 'Comment' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'comments'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'element'  ( refToType 'Comment' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Parameter'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEParameter
			false false false false
			#(#supertypes #(#refToType 'CommentedElement' 'Core'))
			nil
			(attributes
			  ( relationship 'element'
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
				( refToInverseRelationship 'parameters'  ( refToType 'ParametrizedElement' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'element'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'parameters'  ( refToType 'ParametrizedElement' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'ParametrizedElement'
			nil nil
			nil
			nil
			true false false
			nil nil
			CODEParametrizedElement
			false false false false
			#(#supertypes #(#refToType 'CommentedElement' 'Core'))
			nil
			(attributes
			  ( relationship 'parameters'
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
				( refToInverseRelationship 'element'  ( refToType 'Parameter' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'parameters'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'element'  ( refToType 'Parameter' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Module'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEModule
			false false false false
			#(#supertypes #(#refToType 'ParametrizedElement' 'Core'))
			nil
			(attributes
			  ( relationship 'subModules'
				nil nil
				nil
				nil
				AGGREGATES 'subModules'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superModule'  ( refToType 'Module' 'Core'  )  ) 
			   )

			  ( relationship 'superModule'
				nil nil
				nil
				nil
				isAGGREGATED 'superModule'
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subModules'  ( refToType 'Module' 'Core'  )  ) 
			   )

			  ( relationship 'types'
				nil nil
				nil
				nil
				AGGREGATES 'types'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'module'  ( refToType 'Type' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'superModule'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subModules'  ( refToType 'Module' 'Core'  )  ) 
			   )

			  ( relationship 'subModules'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superModule'  ( refToType 'Module' 'Core'  )  ) 
			   )

			  ( relationship 'types'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'module'  ( refToType 'Type' 'Core'  )  ) 
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
			CODEModel
			false false false false
			#(#supertypes #(#refToType 'Module' 'Core'))
			nil
			nil
			nil
			nil
		   )

		  ( type 'Attribute'
			nil nil
			nil
			(aspects
			   ( refToAspect 'a' 'Core'  ) 
			 )
			false false false
			nil nil
			CODEAttribute
			false false false false
			#(#supertypes #(#refToType 'ParametrizedElement' 'Core'))
			nil
			(attributes
			  ( relationship 'type'
				nil nil
				nil
				nil
				isAGGREGATED 'type'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'attributes'  ( refToType 'Type' 'Core'  )  ) 
			   )

			  ( relationship 'attributeRefinements'
				nil nil
				nil
				nil
				REFERENCES 'attributeRefinements'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'refinedAttributes'  ( refToType 'AttributeRefinement' 'Core'  )  ) 
			   )

			  ( relationship 'valueType'
				nil nil
				nil
				nil
				REFERENCES 'valueType'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'typeOfValues'  ( refToType 'Type' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'attributeRefinements'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'refinedAttributes'  ( refToType 'AttributeRefinement' 'Core'  )  ) 
			   )

			  ( relationship 'type'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'attributes'  ( refToType 'Type' 'Core'  )  ) 
			   )

			  ( relationship 'valueType'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'typeOfValues'  ( refToType 'Type' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'AttributeRefinement'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEAttributeRefinement
			false false false false
			#(#supertypes #(#refToType 'Attribute' 'Core'))
			nil
			(attributes
			  ( relationship 'refinedAttributes'
				nil nil
				nil
				nil
				REFERENCES 'refinedAttributes'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'attributeRefinements'  ( refToType 'Attribute' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'refinedAttributes'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'attributeRefinements'  ( refToType 'Attribute' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Relationship'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODERelationship
			false false false false
			#(#supertypes #(#refToType 'ParametrizedElement' 'Core'))
			nil
			(attributes
			  ( relationship 'type'
				nil nil
				nil
				nil
				isAGGREGATED 'type'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relationships'  ( refToType 'Type' 'Core'  )  ) 
			   )

			  ( relationship 'relationshipRefinements'
				nil nil
				nil
				nil
				REFERENCES 'relationshipRefinements'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'refinedRelationships'  ( refToType 'RelationshipRefinement' 'Core'  )  ) 
			   )

			  ( relationship 'inverse'
				nil nil
				nil
				nil
				REFERENCES 'inverse'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'inverse'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			  ( relationship 'inverse'
				nil nil
				nil
				nil
				REFERENCES 'inverse'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'inverse'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			  ( relationship 'relatedType'
				nil nil
				nil
				nil
				REFERENCES 'relatedType'
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relatedWith'  ( refToType 'Type' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'inverse'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'inverse'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			  ( relationship 'relationshipRefinements'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'refinedRelationships'  ( refToType 'RelationshipRefinement' 'Core'  )  ) 
			   )

			  ( relationship 'type'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relationships'  ( refToType 'Type' 'Core'  )  ) 
			   )

			  ( relationship 'relatedType'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relatedWith'  ( refToType 'Type' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'RelationshipRefinement'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODERelationshipRefinement
			false false false false
			#(#supertypes #(#refToType 'Relationship' 'Core'))
			nil
			(attributes
			  ( relationship 'refinedRelationships'
				nil nil
				nil
				nil
				REFERENCES 'refinedRelationships'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relationshipRefinements'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'refinedRelationships'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relationshipRefinements'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Type'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEType
			false false false false
			#(#supertypes #(#refToType 'ParametrizedElement' 'Core'))
			nil
			(attributes
			  ( relationship 'module'
				nil nil
				nil
				nil
				isAGGREGATED 'module'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'types'  ( refToType 'Module' 'Core'  )  ) 
			   )

			  ( relationship 'relationships'
				nil nil
				nil
				nil
				AGGREGATES 'relationships'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'type'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			  ( relationship 'attributes'
				nil nil
				nil
				nil
				AGGREGATES 'attributes'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'type'  ( refToType 'Attribute' 'Core'  )  ) 
			   )

			  ( relationship 'superTypes'
				nil nil
				nil
				nil
				REFERENCES 'superTypes'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subTypes'  ( refToType 'Type' 'Core'  )  ) 
			   )

			  ( relationship 'subTypes'
				nil nil
				nil
				nil
				REFERENCES 'subTypes'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superTypes'  ( refToType 'Type' 'Core'  )  ) 
			   )

			  ( relationship 'relatedWith'
				nil nil
				nil
				nil
				REFERENCES 'relatedWith'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relatedType'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			  ( relationship 'typeOfValues'
				nil nil
				nil
				nil
				REFERENCES 'typeOfValues'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'valueType'  ( refToType 'Attribute' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'module'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'types'  ( refToType 'Module' 'Core'  )  ) 
			   )

			  ( relationship 'attributes'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'type'  ( refToType 'Attribute' 'Core'  )  ) 
			   )

			  ( relationship 'typeOfValues'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'valueType'  ( refToType 'Attribute' 'Core'  )  ) 
			   )

			  ( relationship 'relationships'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'type'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			  ( relationship 'relatedWith'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relatedType'  ( refToType 'Relationship' 'Core'  )  ) 
			   )

			  ( relationship 'subTypes'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superTypes'  ( refToType 'Type' 'Core'  )  ) 
			   )

			  ( relationship 'superTypes'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subTypes'  ( refToType 'Type' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'DataTypes'
		nil nil
		nil
		nil
		nil nil
		(types
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

		  ( type 'Symbol'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			#(#supertypes #(#refToType 'String' 'DataTypes'))
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

		 )
		nil
	   )

	 )
	nil nil
   )! !

!CMSourceModel class publicMethodsFor: 'navigation'!

modelEditorMETAPerspectives

	^self modelEditorMETAPerspectivesSourceModel, self modelEditorMETAPerspectivesGeneral, 
		super modelEditorMETAPerspectives!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Name')));
		yourself!

modelEditorMETAPerspectivesSourceModel
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Model'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Model')));
		yourself!

modelEditorMETASelectors

	^super modelEditorMETASelectors , self modelEditorMETASelectorsSourceModel!

modelEditorMETASelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMView selector: #modelEditorMETASelectorsGeneral target: nil selector: nil."

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
		yourself!

modelEditorMETASelectorsSourceModel

	"METAChildSpecAutoViewEditor openOn: CMSourceModel selector: #modelEditorMETASelectorsSourceModel target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 3)
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Model';
			basicSelector: #model;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Model';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEModel;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		yourself!

modelEditorPathSelectors

	| aLocalView |
	aLocalView := self modelEditorPathSelectorsSourceModel.

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: aLocalView metaSelectorsSelector;
		pathSelectorsSelector: aLocalView pathSelectorsSelector;
		metaSelectorsToSelect: aLocalView metaSelectorsToSelect, super modelEditorPathSelectors metaSelectorsToSelect!

modelEditorPathSelectorsSourceModel

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #( 'Model')! !

!CMSourceModel class publicMethodsFor: 'ojo'!

ojoModel! !

!CMSourceModel publicMethodsFor: 'accessing'!

model
	^model!

model: theModel
	model := theModel!

name
	^self model isNil ifTrue: ['unknown model' copy] ifFalse: [ self model name]! !

!CMSourceModel publicMethodsFor: 'navigation'!

displayName
	^self name!

metaNameSelector
	^#name! !

!CMView class publicMethodsFor: 'examples'!

exampleViewForCMViews
	"CMView exampleViewForCMViews"
	"CMView exampleViewForCMViews browsePath"
	"CMView exampleViewForCMViews newRootNodeForObject: CMSourceModel exampleSourceModelInstanceForCMViews"
	"(CMView exampleViewForCMViews newRootNodeForObject: CMSourceModel exampleSourceModelInstanceForCMViews) children"

	| aViewModel aView aSourceModel aModelMap |

	aSourceModel 	:= CMSourceModel exampleSourceModelForCMViews.
	aViewModel 		:= CMViewModel exampleViewModelForCMViews.
	aModelMap 		:= CMModelMap exampleModelMapForCMViews.


	aView := self named: 'ExampleViewForCMViews' viewModel: aViewModel modelMap: aModelMap sourceModel: aSourceModel.
	^aView! !

!CMView class publicMethodsFor: 'instance creation'!

named: theName viewModel: theViewModel modelMap: theModelMap sourceModel: theSourceModel 
	"CMView named: 'Test' viewModel: theViewModel modelMap: theModelMap sourceModel: theSourceModel"

	| aView |

	theName isNil ifTrue: [ ^nil].
	theViewModel isNil ifTrue: [ ^nil].
	theModelMap isNil ifTrue: [ ^nil].
	theSourceModel isNil ifTrue: [ ^nil].

	aView := self new.
	aView name: theName viewModel: theViewModel modelMap: theModelMap sourceModel: theSourceModel.
	^aView! !

!CMView class publicMethodsFor: 'navigation'!

modelEditorMETAPerspectives

	^self modelEditorMETAPerspectivesView, 
		super modelEditorMETAPerspectives!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Name')));
		yourself!

modelEditorMETAPerspectivesView
	^OrderedCollection new

		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'RootNodesManager'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'RootNodesManager')));
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'SourceModel'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'SourceModel')));
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'ModelMap'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'ModelMap')));
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'ViewModel'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'ViewModel')));
		yourself!

modelEditorMETASelectors

	^super modelEditorMETASelectors , self modelEditorMETASelectorsView!

modelEditorMETASelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMView selector: #modelEditorMETASelectorsGeneral target: nil selector: nil."

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
		yourself!

modelEditorMETASelectorsView

	"METAChildSpecAutoViewEditor openOn: CMView selector: #modelEditorMETASelectorsView target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 3)
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'RootNodesManager';
			basicSelector: #rootNodesManager;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'RootNodesManager';
			displaySelector: #printString;
			canShowInTree: true;
			objectClassName: #CMSourceModel;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'SourceModel';
			basicSelector: #sourceModel;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'SourceModel';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CMSourceModel;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ModelMap';
			basicSelector: #modelMap;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ModelMap';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CMModelMap;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ViewModel';
			basicSelector: #viewModel;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: false;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ViewModel';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CMViewModel;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		yourself!

modelEditorPathSelectors

	| aLocalView |
	aLocalView := self modelEditorPathSelectorsView.

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: aLocalView metaSelectorsSelector;
		pathSelectorsSelector: aLocalView pathSelectorsSelector;
		metaSelectorsToSelect: aLocalView metaSelectorsToSelect, super modelEditorPathSelectors metaSelectorsToSelect!

modelEditorPathSelectorsView

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #('RootNodesManager' 'SourceModel'  'ModelMap'  'ViewModel')! !

!CMView publicMethodsFor: 'accessing'!

modelMap
	^modelMap!

name
	^name!

sourceModel
	^sourceModel!

sourceObjectDeletedAspects
	sourceObjectDeletedAspects isNil ifTrue: [ self initSourceObjectDeletedAspects].
	^sourceObjectDeletedAspects!

viewModel
	^viewModel! !

!CMView publicMethodsFor: 'accessing-adaptors'!

attributeAdaptorFactory
	^attributeAdaptorFactory!

attributeAdaptorFactoryClass
	^attributeAdaptorFactoryClass!

classAdaptorFactory
	^classAdaptorFactory!

classAdaptorFactoryClass
	^classAdaptorFactoryClass!

collectionAdaptorFactory
	^collectionAdaptorFactory!

collectionAdaptorFactoryClass
	^collectionAdaptorFactoryClass!

orderedAdaptorFactory
	^orderedAdaptorFactory!

orderedAdaptorFactoryClass
	^orderedAdaptorFactoryClass! !

!CMView publicMethodsFor: 'collaborators'!

dependencyManagement
	dependencyManagement isNil ifTrue: [ self initDependencyManagement].
	^dependencyManagement!

rootNodesManager
	rootNodesManager isNil ifTrue: [ self initRootNodesManager].
	^rootNodesManager! !

!CMView publicMethodsFor: 'initialize-collaborators'!

initDependencyManagement
	dependencyManagement := self  preferredDependencyManagementClass newForView: self.!

initRootNodesManager
	rootNodesManager := self  preferredRootNodesManagerClass newForView: self.!

initRootNodesManager: theRootNodesManager
	rootNodesManager := theRootNodesManager! !

!CMView publicMethodsFor: 'initialize-release'!

initSourceObjectDeletedAspects
	sourceObjectDeletedAspects := #(objectDeleted objectDisconnectedOfTree)!

name: theName viewModel: theViewModel modelMap: theModelMap sourceModel: theSourceModel 

	| aModelMap aSourceModel aViewModel |
	name				:=  theName.
	viewModel 		:= theViewModel.
	modelMap 		:= theModelMap.
	sourceModel 	:= theSourceModel.

	theModelMap isNil ifFalse: [ 
		aModelMap := theModelMap map.
		aSourceModel := theSourceModel isNil ifTrue: [ nil] ifFalse: [ theSourceModel model].
		aViewModel := theViewModel isNil ifTrue: [ nil] ifFalse: [ theViewModel model].

		aModelMap isNil ifFalse: [ aModelMap bindToSourceModel: aSourceModel viewModel: aViewModel]]!

release
	self releaseRootNodesManager.

	name := nil.
	viewModel := nil.
	sourceModel := nil.
	modelMap := nil.
	rootNodesManager := nil.
	attributeAdaptorFactoryClass := nil.
	attributeAdaptorFactory := nil.
	classAdaptorFactoryClass := nil.
	classAdaptorFactory := nil.
	collectionAdaptorFactoryClass := nil.
	collectionAdaptorFactory := nil.
	orderedAdaptorFactoryClass := nil.
	orderedAdaptorFactory := nil.
	dependencyManagement := nil.

	super release!

releaseRootNodesManager
	rootNodesManager isNil ifFalse: [ 
		rootNodesManager release
	].
	rootNodesManager := nil! !

!CMView publicMethodsFor: 'navigation'!

displayName
	^self name!

metaNameSelector
	^#name! !

!CMView publicMethodsFor: 'svces'!

newRootNodeForObject: theObject
	| aRootNodesManager |
	theObject isNil ifTrue: [ ^nil].

	aRootNodesManager := self rootNodesManager.
	aRootNodesManager isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView rootsManager isNil'].

	^aRootNodesManager newRootNodeForObject: theObject! !

!CMViewCollaborator class publicMethodsFor: 'navigation'!

browserParameters

	^nil!

browserParametersForModelEditor

	^Dictionary new
		at:  METABrowser showCanvasLabelParameterSymbol put: true;
		at:  METABrowser editorsOpenerParameterSymbol put: CODEMModelEditorsOpener editorsOpener;
		at:  METABrowser numberOfEditorHoldersParameterSymbol put: 5;

		yourself!

defaultBrowserParameters
	^self browserParametersForModelEditor!

defaultDefinitionsHolder
	^CODEMModelDefinitionsHolder forModelEditor!

metaPerspectives

	^(Smalltalk at: #METAChildSpec ifAbsent: [nil]) autoMETAPerspectivesFrom: self!

metaSelectors
	^self modelEditorMETASelectors!

modelEditorMETAPerspectives

	^self modelEditorMETAPerspectivesGeneral!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #()));
		yourself!

modelEditorMETASelectors

	^self modelEditorMETASelectorsGeneral!

modelEditorMETASelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMViewCollaborator selector: #modelEditorMETASelectorsGeneral target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 5)
		yourself!

modelEditorPathSelectors

	^self modelEditorPathSelectorsGeneral!

modelEditorPathSelectorsGeneral

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #()!

pathSelectors
	^self metaSelectors! !

!CMViewCollaborator class publicMethodsFor: 'preferences'!

preferredAdaptorClass
	^self preferredPreferencesClass preferredAdaptorClass!

preferredAdaptorFactoryClass
	^self preferredPreferencesClass preferredAdaptorFactoryClass!

preferredAdaptorFactoryFinderClass
	^self preferredPreferencesClass preferredAdaptorFactoryFinderClass!

preferredAttributeAdaptorClass
	^self preferredPreferencesClass preferredAttributeAdaptorClass!

preferredAttributeAdaptorFactoryClass
	^self preferredPreferencesClass preferredAttributeAdaptorFactoryClass!

preferredAttributeAdaptorFactoryFinderClass
	^self preferredPreferencesClass preferredAttributeAdaptorFactoryFinderClass!

preferredAttributeChildrenNodesFactoryClass
	^self preferredPreferencesClass preferredAttributeChildrenNodesFactoryClass!

preferredAttributeNodeClass
	^self preferredPreferencesClass preferredAttributeNodeClass!

preferredAttributeNodeFactoryClass
	^self preferredPreferencesClass preferredAttributeNodeFactoryClass!

preferredAttributeNodeFactoryFinderClass
	^self preferredPreferencesClass preferredAttributeNodeFactoryFinderClass!

preferredChildrenNodeClass
	^self preferredPreferencesClass preferredChildrenNodeClass!

preferredChildrenNodesFactoryClass
	^self preferredPreferencesClass preferredChildrenNodesFactoryClass!

preferredChildrenNodesFactoryFinderClass
	^self preferredPreferencesClass preferredChildrenNodesFactoryFinderClass!

preferredClassAdaptorClass
	^self preferredPreferencesClass preferredClassAdaptorClass!

preferredClassAdaptorFactoryClass
	^self preferredPreferencesClass preferredClassAdaptorFactoryClass!

preferredClassAdaptorFactoryFinderClass
	^self preferredPreferencesClass preferredClassAdaptorFactoryFinderClass!

preferredClassChildrenNodesFactoryClass
	^self preferredPreferencesClass preferredClassChildrenNodesFactoryClass!

preferredClassChildrenNodesFactoryFinderClass
	^self preferredPreferencesClass preferredClassChildrenNodesFactoryFinderClass!

preferredClassNodeClass
	^self preferredPreferencesClass preferredClassNodeClass!

preferredClassNodeFactoryClass
	^self preferredPreferencesClass preferredClassNodeFactoryClass!

preferredClassNodeFactoryFinderClass
	^self preferredPreferencesClass preferredClassNodeFactoryFinderClass!

preferredCollectionAdaptorClass
	^self preferredPreferencesClass preferredCollectionAdaptorClass!

preferredCollectionAdaptorFactoryClass
	^self preferredPreferencesClass preferredCollectionAdaptorFactoryClass!

preferredCollectionAdaptorFactoryFinderClass
	^self preferredPreferencesClass preferredCollectionAdaptorFactoryFinderClass!

preferredCollectionChildrenNodesFactoryClass
	^self preferredPreferencesClass preferredCollectionChildrenNodesFactoryClass!

preferredCollectionChildrenNodesFactoryFinderClass
	^self preferredPreferencesClass preferredCollectionChildrenNodesFactoryFinderClass!

preferredCollectionNodeClass
	^self preferredPreferencesClass preferredCollectionNodeClass!

preferredCollectionNodeFactoryClass
	^self preferredPreferencesClass preferredCollectionNodeFactoryClass!

preferredCollectionNodeFactoryFinderClass
	^self preferredPreferencesClass preferredCollectionNodeFactoryFinderClass!

preferredDependencyManagementClass
	^self preferredPreferencesClass preferredDependencyManagementClass!

preferredMetaInfoLikehoodEvaluatorClass
	^self preferredPreferencesClass preferredMetaInfoLikehoodEvaluatorClass!

preferredObjectAdaptorClass
	^self preferredPreferencesClass preferredObjectAdaptorClass!

preferredObjectAdaptorFactoryClass
	^self preferredPreferencesClass preferredObjectAdaptorFactoryClass!

preferredObjectAdaptorFactoryFinderClass
	^self preferredPreferencesClass preferredObjectAdaptorFactoryFinderClass!

preferredObjectChildrenNodesFactoryClass
	^self preferredPreferencesClass preferredObjectChildrenNodesFactoryClass!

preferredObjectChildrenNodesFactoryFinderClass
	^self preferredPreferencesClass preferredObjectChildrenNodesFactoryFinderClass!

preferredObjectNodeClass
	^self preferredPreferencesClass preferredObjectNodeClass!

preferredObjectNodeFactoryClass
	^self preferredPreferencesClass preferredObjectNodeFactoryClass!

preferredObjectNodeFactoryFinderClass
	^self preferredPreferencesClass preferredObjectNodeFactoryFinderClass!

preferredOrderedAdaptorClass
	^self preferredPreferencesClass preferredOrderedAdaptorClass!

preferredOrderedAdaptorFactoryClass
	^self preferredPreferencesClass preferredOrderedAdaptorFactoryClass!

preferredOrderedAdaptorFactoryFinderClass
	^self preferredPreferencesClass preferredOrderedAdaptorFactoryFinderClass!

preferredOrderedChildrenNodesFactoryClass
	^self preferredPreferencesClass preferredOrderedChildrenNodesFactoryClass!

preferredOrderedChildrenNodesFactoryFinderClass
	^self preferredPreferencesClass preferredOrderedChildrenNodesFactoryFinderClass!

preferredOrderedNodeClass
	^self preferredPreferencesClass preferredOrderedNodeClass!

preferredOrderedNodeFactoryClass
	^self preferredPreferencesClass preferredOrderedNodeFactoryClass!

preferredOrderedNodeFactoryFinderClass
	^self preferredPreferencesClass preferredOrderedNodeFactoryFinderClass!

preferredPreferencesClass
	^CMPreferences!

preferredRootAdaptorClass
	^self preferredPreferencesClass preferredRootAdaptorClass!

preferredRootAdaptorFactoryClass
	^self preferredPreferencesClass preferredRootAdaptorFactoryClass!

preferredRootAdaptorFactoryFinderClass
	^self preferredPreferencesClass preferredRootAdaptorFactoryFinderClass!

preferredRootChildrenNodesFactoryClass
	^self preferredPreferencesClass preferredRootChildrenNodesFactoryClass!

preferredRootChildrenNodesFactoryFinderClass
	^self preferredPreferencesClass preferredRootChildrenNodesFactoryFinderClass!

preferredRootNodeClass
	^self preferredPreferencesClass preferredRootNodeClass!

preferredRootNodeFactoryClass
	^self preferredPreferencesClass preferredRootNodeFactoryClass!

preferredRootNodeFactoryFinderClass
	^self preferredPreferencesClass preferredRootNodeFactoryFinderClass!

preferredRootNodesManagerClass
	^self preferredPreferencesClass preferredRootNodesManagerClass!

preferredRootsManagerClass
	^self preferredPreferencesClass preferredRootsManagerClass!

preferredViewMetaInfoUtilsClass
	^self preferredPreferencesClass preferredViewMetaInfoUtilsClass!

preferredViewRootNodeFactoryClass
	^self preferredPreferencesClass preferredViewRootNodeFactoryClass! !

!CMViewCollaborator publicMethodsFor: 'navigation'!

browse
	^METAScopedApplicationBrowser
		openForObject: 			self 
		definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:	self defaultBrowserParameters!

browsePath

	^CODEMModelGenericBrowser
		openForObject: 			self 
		definitionsHolder: 		self defaultDefinitionsHolder
		browserParameters:	self defaultBrowserParameters!

defaultBrowserParameters
	^self class defaultBrowserParameters!

defaultDefinitionsHolder
	^self class defaultDefinitionsHolder!

displayName
	^'a', self class!

metaClass

	^self class!

metaEditorClassLabel

	^self classLabelForMETAEditor!

metaNameSelector
	^#printString!

metaSelectorsSelector
	^#metaSelectors! !

!CMViewCollaborator publicMethodsFor: 'preferences'!

preferredAdaptorClass
	^self class preferredAdaptorClass!

preferredAdaptorFactoryClass
	^self class preferredAdaptorFactoryClass!

preferredAdaptorFactoryFinderClass
	^self class preferredAdaptorFactoryFinderClass!

preferredAttributeAdaptorClass
	^self class preferredAttributeAdaptorClass!

preferredAttributeAdaptorFactoryClass
	^self class preferredAttributeAdaptorFactoryClass!

preferredAttributeAdaptorFactoryFinderClass
	^self class preferredAttributeAdaptorFactoryFinderClass!

preferredAttributeChildrenNodesFactoryClass
	^self class preferredAttributeChildrenNodesFactoryClass!

preferredAttributeNodeClass
	^self class preferredAttributeNodeClass!

preferredAttributeNodeFactoryClass
	^self class preferredAttributeNodeFactoryClass!

preferredAttributeNodeFactoryFinderClass
	^self class preferredAttributeNodeFactoryFinderClass!

preferredChildrenNodeClass
	^self class preferredChildrenNodeClass!

preferredChildrenNodesFactoryClass
	^self class preferredChildrenNodesFactoryClass!

preferredChildrenNodesFactoryFinderClass
	^self class preferredChildrenNodesFactoryFinderClass!

preferredClassAdaptorClass
	^self class preferredClassAdaptorClass!

preferredClassAdaptorFactoryClass
	^self class preferredClassAdaptorFactoryClass!

preferredClassAdaptorFactoryFinderClass
	^self class preferredClassAdaptorFactoryFinderClass!

preferredClassChildrenNodesFactoryClass
	^self class preferredClassChildrenNodesFactoryClass!

preferredClassChildrenNodesFactoryFinderClass
	^self class preferredClassChildrenNodesFactoryFinderClass!

preferredClassNodeClass
	^self class preferredClassNodeClass!

preferredClassNodeFactoryClass
	^self class preferredClassNodeFactoryClass!

preferredClassNodeFactoryFinderClass
	^self class preferredClassNodeFactoryFinderClass!

preferredCollectionAdaptorClass
	^self class preferredCollectionAdaptorClass!

preferredCollectionAdaptorFactoryClass
	^self class preferredCollectionAdaptorFactoryClass!

preferredCollectionAdaptorFactoryFinderClass
	^self class preferredCollectionAdaptorFactoryFinderClass!

preferredCollectionChildrenNodesFactoryClass
	^self class preferredCollectionChildrenNodesFactoryClass!

preferredCollectionChildrenNodesFactoryFinderClass
	^self class preferredCollectionChildrenNodesFactoryFinderClass!

preferredCollectionNodeClass
	^self class preferredCollectionNodeClass!

preferredCollectionNodeFactoryClass
	^self class preferredCollectionNodeFactoryClass!

preferredCollectionNodeFactoryFinderClass
	^self class preferredCollectionNodeFactoryFinderClass!

preferredDependencyManagementClass
	^self class preferredDependencyManagementClass!

preferredMetaInfoLikehoodEvaluatorClass
	^self class preferredMetaInfoLikehoodEvaluatorClass!

preferredObjectAdaptorClass
	^self class preferredObjectAdaptorClass!

preferredObjectAdaptorFactoryClass
	^self class preferredObjectAdaptorFactoryClass!

preferredObjectAdaptorFactoryFinderClass
	^self class preferredObjectAdaptorFactoryFinderClass!

preferredObjectChildrenNodesFactoryClass
	^self class preferredObjectChildrenNodesFactoryClass!

preferredObjectChildrenNodesFactoryFinderClass
	^self class preferredObjectChildrenNodesFactoryFinderClass!

preferredObjectNodeClass
	^self class preferredObjectNodeClass!

preferredObjectNodeFactoryClass
	^self class preferredObjectNodeFactoryClass!

preferredObjectNodeFactoryFinderClass
	^self class preferredObjectNodeFactoryFinderClass!

preferredObjectNodesFactoryClass
	^self class preferredObjectNodeFactoryClass!

preferredOrderedAdaptorClass
	^self class preferredOrderedAdaptorClass!

preferredOrderedAdaptorFactoryClass
	^self class preferredOrderedAdaptorFactoryClass!

preferredOrderedAdaptorFactoryFinderClass
	^self class preferredOrderedAdaptorFactoryFinderClass!

preferredOrderedChildrenNodesFactoryFinderClass
	^self class preferredOrderedChildrenNodesFactoryFinderClass!

preferredOrderedNodeClass
	^self class preferredOrderedNodeClass!

preferredOrderedNodeFactoryClass
	^self class preferredOrderedNodeFactoryClass!

preferredRootAdaptorClass
	^self class preferredRootAdaptorClass!

preferredRootAdaptorFactoryClass
	^self class preferredRootAdaptorFactoryClass!

preferredRootAdaptorFactoryFinderClass
	^self class preferredRootAdaptorFactoryFinderClass!

preferredRootChildrenNodesFactoryClass
	^self class preferredRootChildrenNodesFactoryClass!

preferredRootChildrenNodesFactoryFinderClass
	^self class preferredRootChildrenNodesFactoryFinderClass!

preferredRootNodeClass
	^self class preferredRootNodeClass!

preferredRootNodeFactoryClass
	^self class preferredRootNodeFactoryClass!

preferredRootNodeFactoryFinderClass
	^self class preferredRootNodeFactoryFinderClass!

preferredRootNodesManagerClass
	^self class preferredRootNodesManagerClass!

preferredViewMetaInfoUtilsClass
	^self class preferredViewMetaInfoUtilsClass!

preferredViewRootNodeFactoryClass
	^self class preferredViewRootNodeFactoryClass! !

!CMViewMetaInfoUtils class publicMethodsFor: 'utils'!

confirmMetaInfo: theMetaInfo forSourceObject: theObject inView: theView

	| aModel aSourceCMModel aSourceModel |

	theObject isNil ifFalse: [ ^nil].
	theMetaInfo isNil ifFalse: [ ^nil].
	theView isNil ifFalse: [ ^nil].

	aModel := theMetaInfo model.
	aModel isNil ifFalse: [ ^nil].

	aSourceCMModel := theView sourceModel.
	aSourceCMModel isNil ifFalse: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView sourceModel isNil'].

	aSourceModel := aSourceCMModel model.
	aSourceModel isNil ifFalse: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel model isNil'].

	^aModel == aSourceCMModel ifTrue:  [ theMetaInfo] ifFalse: [ nil]!

confirmMetaInfoReference: theMetaInfoReference forSourceObject: theObject inView: theView

	| aMetaInfo aSourceCMModel aSourceModel |

	theObject isNil ifTrue: [ ^nil].
	theMetaInfoReference isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	aSourceCMModel := theView sourceModel.
	aSourceCMModel isNil ifFalse: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView sourceModel isNil'].

	aSourceModel := aSourceCMModel model.
	aSourceModel isNil ifFalse: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel model isNil'].

	aMetaInfo := CODEElement resolveReferencedElementFromPersistenceAsCode: theMetaInfoReference solver: aSourceModel.
	aMetaInfo isNil ifTrue: [ ^nil].

	self setMetaInfo: aMetaInfo inObject: theObject.

	^aMetaInfo!

findElementMapForObject: theObject withSourceMetaInfo: theSourceMetaInfo  inView: theView

	| aMap anElementMap aModelMap |
	aModelMap := theView modelMap.
	aModelMap isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView modelMap isNil'].

	aMap := aModelMap map.
	aMap isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aModelMap map isNil'].

	anElementMap := self scanMapsFolder: aMap forElementMapWithSourceMetaInfo: theSourceMetaInfo forSourceObject: theObject inView: theView.
	^anElementMap!

findMetaInfoForSourceObject: theObject inView: theView

	| anObjectMetaInfo anObjectMetaInfoReference aSourceModel |
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	aSourceModel := theView sourceModel.
	aSourceModel isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView sourceModel isNil'].

	anObjectMetaInfo := self getMetaInfoFromObject: theObject.
	anObjectMetaInfo isNil ifFalse: [ 
		^self confirmMetaInfo: anObjectMetaInfo forSourceObject: theObject inView: theView
	].

	anObjectMetaInfoReference := self getMetaInfoReferenceFromObject: theObject.
	anObjectMetaInfoReference isNil ifFalse: [ 
		^self confirmMetaInfoReference: anObjectMetaInfo forSourceObject: theObject inView: theView
	].

	^self scanModelForSourceObject: theObject inView: theView!

findMetaInfoForSourceObject: theObject inView: theView withLikehoodEvaluator: theLikehoodEvaluator

	| anObjectMetaInfo anObjectMetaInfoReference aSourceModel |
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	aSourceModel := theView sourceModel.
	aSourceModel isNil ifTrue: [ ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aView sourceModel isNil'].

	anObjectMetaInfo := self getMetaInfoFromObject: theObject.
	anObjectMetaInfo isNil ifFalse: [ 
		^self confirmMetaInfo: anObjectMetaInfo forSourceObject: theObject inView: theView
	].

	anObjectMetaInfoReference := self getMetaInfoReferenceFromObject: theObject.
	anObjectMetaInfoReference isNil ifFalse: [ 
		^self confirmMetaInfoReference: anObjectMetaInfo forSourceObject: theObject inView: theView
	].

	^self scanModelForSourceObject: theObject inView: theView withLikehoodEvaluator: theLikehoodEvaluator!

getMetaInfoFromObject: theObject
	theObject isNil ifTrue: [ ^nil].

	^Object messageNotUnderstoodSignal 
		handle: [:anException | anException returnWith: nil]
		do: [ theObject getMetaInfo].!

getMetaInfoReferenceFromObject: theObject
	theObject isNil ifTrue: [ ^nil].

	^Object messageNotUnderstoodSignal 
		handle: [:anException | anException returnWith: nil]
		do: [ theObject getMetaInfoReference].!

scanElementMap: theElementMap forElementMapWithSourceMetaInfo: theMetaInfo forNode: theNode

	| anElementMap someElementMaps aFoundEM |
	theElementMap isNil ifTrue: [ ^nil].
	theMetaInfo isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	someElementMaps := theElementMap subElementMaps.
	(someElementMaps isNil or: [ someElementMaps isEmpty]) ifTrue: [ ^nil].
	
	anElementMap := someElementMaps detect: [:anEM | anEM viewElementsIncludes: theMetaInfo] ifNone: [ nil].
	anElementMap isNil ifFalse: [ ^anElementMap].

	aFoundEM := nil.
	someElementMaps detect: [:anEM |  
		aFoundEM := self scanElementMap: anEM forElementMapWithSourceMetaInfo: theMetaInfo forNode: theNode.
		aFoundEM isNil not
	] ifNone: [ nil].
	aFoundEM isNil ifFalse: [ ^aFoundEM].

	
	^nil!

scanElementMap: theElementMap forElementMapWithSourceMetaInfo: theMetaInfo forSourceObject: theObject inView: theView

	| anElementMap someElementMaps aFoundEM |
	theElementMap isNil ifTrue: [ ^nil].
	theMetaInfo isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	someElementMaps := theElementMap subElementMaps.
	(someElementMaps isNil or: [ someElementMaps isEmpty]) ifTrue: [ ^nil].
	
	anElementMap := someElementMaps detect: [:anEM | anEM sourceElementsIncludes: theMetaInfo] ifNone: [ nil].
	anElementMap isNil ifFalse: [ ^anElementMap].

	aFoundEM := nil.
	someElementMaps detect: [:anEM |  
		aFoundEM := self scanElementMap: anEM forElementMapWithSourceMetaInfo: theMetaInfo forSourceObject: theObject inView: theView.
		aFoundEM isNil not
	] ifNone: [ nil].
	aFoundEM isNil ifFalse: [ ^aFoundEM].

	
	^nil!

scanElementMap: theElementMap forElementMapWithViewMetaInfo: theViewMetaInfo forNode: theNode

	| someSubElementMaps aSourceElementMap aFoundElementMap |

	theNode isNil ifTrue: [ ^nil].
	theViewMetaInfo isNil ifTrue: [ ^nil].
	theElementMap isNil ifTrue: [ ^nil].

	someSubElementMaps := theElementMap subElementMaps.
	(someSubElementMaps isNil or: [ someSubElementMaps isEmpty]) ifTrue: [ ^nil].
	
	aSourceElementMap := someSubElementMaps detect: [:anElementMap |
		anElementMap viewElementsIncludes: theViewMetaInfo] ifNone: [ nil].
	aSourceElementMap  isNil ifFalse: [ ^aSourceElementMap].

	aSourceElementMap := someSubElementMaps detect: [:anElementMap |
		(anElementMap viewElements detect: [:aR | aR inverse == theViewMetaInfo inverse] ifNone: [ nil]) isNil not] ifNone: [ nil].
	aSourceElementMap  isNil ifFalse: [ ^aSourceElementMap].

	aFoundElementMap :=  nil.
	someSubElementMaps detect: [:anElementMap |   
		aFoundElementMap := self scanElementMap: anElementMap forElementMapWithViewMetaInfo: theViewMetaInfo forNode: theNode.
		aFoundElementMap isNil not ] ifNone: [ nil].

	^aFoundElementMap!

scanMapsFolder: theMapsFolder forElementMapWithSourceMetaInfo: theMetaInfo forSourceObject: theObject inView: theView

| anElementMap someElementMaps aFoundEM someSubModules |
	theMapsFolder isNil ifTrue: [ ^nil].
	theMetaInfo isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	someElementMaps := theMapsFolder elementMaps.
	(someElementMaps isNil or: [ someElementMaps isEmpty]) ifFalse: [ 
	
		anElementMap := someElementMaps detect: [:anEM | anEM sourceElementsIncludes: theMetaInfo] ifNone: [ nil].
		anElementMap isNil ifFalse: [ ^anElementMap].

		aFoundEM := nil.
		someElementMaps detect: [:anEM |  
			aFoundEM := self scanElementMap: anEM forElementMapWithSourceMetaInfo: theMetaInfo forSourceObject: theObject inView: theView.
			aFoundEM isNil not
		] ifNone: [ nil].
		aFoundEM isNil ifFalse: [ ^aFoundEM]
	].
	
	aFoundEM := nil.
	someSubModules := theMapsFolder subModules.
	someSubModules detect: [:aSM | 
		aFoundEM := self scanMapsFolder: aSM forElementMapWithSourceMetaInfo: theMetaInfo forSourceObject: theObject inView: theView.
		aFoundEM isNil not
	] ifNone: [ nil].
	aFoundEM isNil ifFalse: [ ^aFoundEM].
	
	^nil!

scanMapsFolder: theMapsFolder forElementMapWithViewMetaInfo: theMetaInfo forNode: theNode

| anElementMap someElementMaps aFoundEM someSubModules |

	theMapsFolder isNil ifTrue: [ ^nil].
	theMetaInfo isNil ifTrue: [ ^nil].
	theNode isNil ifTrue: [ ^nil].

	someElementMaps := theMapsFolder elementMaps.
	(someElementMaps isNil or: [ someElementMaps isEmpty]) ifFalse: [ 
	
		anElementMap := someElementMaps detect: [:anEM | anEM viewElementsIncludes: theMetaInfo] ifNone: [ nil].
		anElementMap isNil ifFalse: [ ^anElementMap].

		aFoundEM := nil.
		someElementMaps detect: [:anEM |  
			aFoundEM := self scanElementMap: anEM forElementMapWithViewMetaInfo: theMetaInfo forNode: theNode.
			aFoundEM isNil not
		] ifNone: [ nil].
		aFoundEM isNil ifFalse: [ ^aFoundEM]
	].
	
	aFoundEM := nil.
	someSubModules := theMapsFolder subModules.
	someSubModules detect: [:aSM | 
		aFoundEM := self scanMapsFolder: aSM forElementMapWithViewMetaInfo: theMetaInfo forNode: theNode.
		aFoundEM isNil not
	] ifNone: [ nil].
	aFoundEM isNil ifFalse: [ ^aFoundEM].
	
	^nil!

scanModelForSourceObject: theObject inView: theView

	| aModel aLikehoodEvaluator aMetaInfo aCMModel |
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].

	aCMModel := theView sourceModel.
	aCMModel isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'theView sourceModel isNil'].
	
	aModel := aCMModel model.
	aModel isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel model isNil'].

	aLikehoodEvaluator := self metaInfoLikehoodEvaluator.
	aLikehoodEvaluator isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel metaInfoLikehoodEvaluator isNil'].

	aMetaInfo := aLikehoodEvaluator scanModel: aModel forSourceObject: theObject inView: theView.
	^aMetaInfo!

scanModelForSourceObject: theObject inView: theView withLikehoodEvaluator: theLikehoodEvaluator

	| aModel aMetaInfo aCMModel |
	theObject isNil ifTrue: [ ^nil].
	theView isNil ifTrue: [ ^nil].
	theLikehoodEvaluator isNil ifTrue: [ ^nil].

	aCMModel := theView sourceModel.
	aCMModel isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'theView sourceModel isNil'].
	
	aModel := aCMModel model.
	aModel isNil ifTrue: [  ^CMSignals cmViewBadSetupSignal raiseErrorString: 'aSourceModel model isNil'].

	aMetaInfo := theLikehoodEvaluator scanModel: aModel forSourceObject: theObject inView: theView.
	^aMetaInfo!

scanNode: theNode forElementMapWithViewMetaInfo: theViewMetaInfo

	| aFoundElementMap anElementMap |

	theNode isNil ifTrue: [ ^nil].
	theViewMetaInfo isNil ifTrue: [ ^nil].

	anElementMap := theNode elementMap.
	anElementMap isNil ifTrue: [  ^CMSignals nodeWithoutElementMapSignal raiseWith: self].

	aFoundElementMap :=  self scanElementMap: anElementMap forElementMapWithViewMetaInfo: theViewMetaInfo forNode: theNode.
	^aFoundElementMap!

setMetaInfo: theMetaInfo inObject: theObject
	theMetaInfo isNil ifTrue: [ ^nil].
	theObject isNil ifTrue: [ ^nil].

	^Object messageNotUnderstoodSignal 
		handle: [:anException | anException returnWith: nil]
		do: [ theObject setMetaInfo: theMetaInfo].! !

!CMViewModel class publicMethodsFor: 'examples'!

exampleViewModelForCMViews

	^self onModel: (CODEElement newFromPersistenceAsCode: self exampleCODEViewModelForCMViews)! !

!CMViewModel class publicMethodsFor: 'instance creation'!

onModel: theModel
	| aViewModel |
	theModel isNil ifTrue: [ ^nil].
	aViewModel := self new.
	aViewModel model: theModel.
	^aViewModel! !

!CMViewModel class publicMethodsFor: 'modelElements persistence'!

exampleCODEViewModelForCMViews

	"(CODEElement newFromPersistenceAsCode: CMViewModel exampleCODEViewModelForCMViews) browsePath"

	self ojoModel.

	^   #( model 'ExampleViewModelForCMViews'
	nil nil
	nil
	nil
	CMViewModel exampleCODEViewModelForCMViews
	nil
	(submodules
	  ( module 'Core'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Element'
			nil nil
			nil
			nil
			true false false
			nil nil
			CODEElement
			false false false false
			nil
			nil
			nil
			nil
			nil
		   )

		  ( type 'Package'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEModule
			false false false false
			#(#supertypes #(#refToType 'Element' 'Core'))
			nil
			(attributes
			  ( relationship 'subPackages'
				nil nil
				nil
				nil
				AGGREGATES 'subModules'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				'identifier'
				''
				''
				''
				( refToInverseRelationship 'superPackage'  ( refToType 'Package' 'Core'  )  ) 
			   )

			  ( relationship 'superPackage'
				nil nil
				nil
				nil
				isAGGREGATED 'supeModule'
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subPackages'  ( refToType 'Package' 'Core'  )  ) 
			   )

			  ( relationship 'objects'
				nil nil
				nil
				nil
				AGGREGATES 'types'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'package'  ( refToType 'Object' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'superPackage'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subPackages'  ( refToType 'Package' 'Core'  )  ) 
			   )

			  ( relationship 'subPackages'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superPackage'  ( refToType 'Package' 'Core'  )  ) 
			   )

			  ( relationship 'objects'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'package'  ( refToType 'Object' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Specification'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEModel
			false false false false
			#(#supertypes #(#refToType 'Package' 'Core'))
			nil
			nil
			nil
			nil
		   )

		  ( type 'Property'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEAttribute
			false false false false
			#(#supertypes #(#refToType 'Element' 'Core'))
			nil
			(attributes
			  ( relationship 'object'
				nil nil
				nil
				nil
				isAGGREGATED 'type'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'properties'  ( refToType 'Object' 'Core'  )  ) 
			   )

			  ( relationship 'valueObject'
				nil nil
				nil
				nil
				REFERENCES 'valueType'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'objectOfProperties'  ( refToType 'Object' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'object'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'properties'  ( refToType 'Object' 'Core'  )  ) 
			   )

			  ( relationship 'valueObject'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'objectOfProperties'  ( refToType 'Object' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Link'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODERelationship
			false false false false
			#(#supertypes #(#refToType 'Element' 'Core'))
			nil
			(attributes
			  ( relationship 'object'
				nil nil
				nil
				nil
				isAGGREGATED 'type'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'links'  ( refToType 'Object' 'Core'  )  ) 
			   )

			  ( relationship 'relatedObject'
				nil nil
				nil
				nil
				REFERENCES 'relatedType'
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'linkedWith'  ( refToType 'Object' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'object'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'links'  ( refToType 'Object' 'Core'  )  ) 
			   )

			  ( relationship 'relatedObject'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'linkedWith'  ( refToType 'Object' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'Object'
			nil nil
			nil
			nil
			false false false
			nil nil
			CODEType
			false false false false
			#(#supertypes #(#refToType 'Element' 'Core'))
			nil
			(attributes
			  ( relationship 'package'
				nil nil
				nil
				nil
				isAGGREGATED 'module'
				#'1' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'objects'  ( refToType 'Package' 'Core'  )  ) 
			   )

			  ( relationship 'links'
				nil nil
				nil
				nil
				AGGREGATES 'relationships'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'object'  ( refToType 'Link' 'Core'  )  ) 
			   )

			  ( relationship 'properties'
				nil nil
				nil
				nil
				AGGREGATES 'attributes'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'object'  ( refToType 'Property' 'Core'  )  ) 
			   )

			  ( relationship 'superObjects'
				nil nil
				nil
				nil
				REFERENCES 'superTypes'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subObjects'  ( refToType 'Object' 'Core'  )  ) 
			   )

			  ( relationship 'subObjects'
				nil nil
				nil
				nil
				REFERENCES 'subTypes'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superObjects'  ( refToType 'Object' 'Core'  )  ) 
			   )

			  ( relationship 'linkedWith'
				nil nil
				nil
				nil
				REFERENCES 'relatedWith'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relatedObject'  ( refToType 'Link' 'Core'  )  ) 
			   )

			  ( relationship 'objectOfProperties'
				nil nil
				nil
				nil
				REFERENCES 'typeOfValues'
				#'0' #*
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'valueObject'  ( refToType 'Property' 'Core'  )  ) 
			   )

			 )
			(relationships
			  ( relationship 'package'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'objects'  ( refToType 'Package' 'Core'  )  ) 
			   )

			  ( relationship 'properties'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'object'  ( refToType 'Property' 'Core'  )  ) 
			   )

			  ( relationship 'objectOfProperties'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'valueObject'  ( refToType 'Property' 'Core'  )  ) 
			   )

			  ( relationship 'links'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'object'  ( refToType 'Link' 'Core'  )  ) 
			   )

			  ( relationship 'linkedWith'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'relatedObject'  ( refToType 'Link' 'Core'  )  ) 
			   )

			  ( relationship 'subObjects'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'superObjects'  ( refToType 'Object' 'Core'  )  ) 
			   )

			  ( relationship 'superObjects'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #'1'
				false false true false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'subObjects'  ( refToType 'Object' 'Core'  )  ) 
			   )

			 )
			nil
		   )

		 )
		nil
	   )

	  ( module 'DataTypes'
		nil nil
		nil
		nil
		nil nil
		(types
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

		  ( type 'Symbol'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			#(#supertypes #(#refToType 'String' 'DataTypes'))
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

		 )
		nil
	   )

	 )
	nil nil
   )! !

!CMViewModel class publicMethodsFor: 'navigation'!

modelEditorMETAPerspectives

	^self modelEditorMETAPerspectivesViewModel, self modelEditorMETAPerspectivesGeneral, 
		super modelEditorMETAPerspectives!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Name')));
		yourself!

modelEditorMETAPerspectivesViewModel
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Model'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #( 'Model')));
		yourself!

modelEditorMETASelectors

	^super modelEditorMETASelectors , self modelEditorMETASelectorsViewModel!

modelEditorMETASelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMView selector: #modelEditorMETASelectorsGeneral target: nil selector: nil."

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
		yourself!

modelEditorMETASelectorsViewModel

	"METAChildSpecAutoViewEditor openOn: CMViewModel selector: #modelEditorMETASelectorsViewModel target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 3)
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Model';
			basicSelector: #model;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Model';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEModel;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: false;
			menuSelector: nil;
			yourself);
		yourself!

modelEditorPathSelectors

	| aLocalView |
	aLocalView := self modelEditorPathSelectorsViewModel.

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: aLocalView metaSelectorsSelector;
		pathSelectorsSelector: aLocalView pathSelectorsSelector;
		metaSelectorsToSelect: aLocalView metaSelectorsToSelect, super modelEditorPathSelectors metaSelectorsToSelect!

modelEditorPathSelectorsViewModel

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #( 'Model')! !

!CMViewModel class publicMethodsFor: 'ojo'!

ojoModel! !

!CMViewModel publicMethodsFor: 'accessing'!

model
	^model!

model: theModel
	model := theModel!

name
	^self model isNil ifTrue: ['unknown model' copy] ifFalse: [ self model name]! !

!CMViewModel publicMethodsFor: 'navigation'!

displayName
	^self name!

metaNameSelector
	^#name! !

CMSignals initializeAfterLoad!
CMViewCollaborator initializeAfterLoad!
CMAdaptor initializeAfterLoad!
CMAbstractObjectAdaptor initializeAfterLoad!
CMObjectAdaptor initializeAfterLoad!
CMRootAdaptor initializeAfterLoad!
CMFeatureAdaptor initializeAfterLoad!
CMAttributeAdaptor initializeAfterLoad!
CMClassAdaptor initializeAfterLoad!
CMCollectionAdaptor initializeAfterLoad!
CMOrderedAdaptor initializeAfterLoad!
CMAdaptorFactory initializeAfterLoad!
CMAbstractObjectAdaptorFactory initializeAfterLoad!
CMObjectAdaptorFactory initializeAfterLoad!
CMRootAdaptorFactory initializeAfterLoad!
CMFeatureAdaptorFactory initializeAfterLoad!
CMAttributeAdaptorFactory initializeAfterLoad!
CMClassAdaptorFactory initializeAfterLoad!
CMCollectionAdaptorFactory initializeAfterLoad!
CMOrderedAdaptorFactory initializeAfterLoad!
CMChildrenNodesFactory initializeAfterLoad!
CMAbstractObjectChildrenNodesFactory initializeAfterLoad!
CMObjectChildrenNodesFactory initializeAfterLoad!
CMRootChildrenNodesFactory initializeAfterLoad!
CMFeatureChildrenNodesFactory initializeAfterLoad!
CMAttributeChildrenNodesFactory initializeAfterLoad!
CMFeatureReferencingObjectsChildrenNodesFactory initializeAfterLoad!
CMClassChildrenNodesFactory initializeAfterLoad!
CMCollectionChildrenNodesFactory initializeAfterLoad!
CMOrderedChildrenNodesFactory initializeAfterLoad!
CMDependencyManagement initializeAfterLoad!
CMMetaInfoLikehoodEvaluator initializeAfterLoad!
CMModelMap initializeAfterLoad!
CMNodeFactory initializeAfterLoad!
CMAbstractObjectNodeFactory initializeAfterLoad!
CMObjectNodeFactory initializeAfterLoad!
CMRootNodeFactory initializeAfterLoad!
CMFeatureNodeFactory initializeAfterLoad!
CMAttributeNodeFactory initializeAfterLoad!
CMClassNodeFactory initializeAfterLoad!
CMCollectionNodeFactory initializeAfterLoad!
CMOrderedNodeFactory initializeAfterLoad!
CMRootNodesManager initializeAfterLoad!
CMSourceModel initializeAfterLoad!
CMView initializeAfterLoad!
CMViewMetaInfoUtils initializeAfterLoad!
CMViewModel initializeAfterLoad!
CODE_META_View initializeAfterLoad!

CODE_META_View loaded!
