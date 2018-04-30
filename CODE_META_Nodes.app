'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Nodes in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Nodes becomeDefault!

Object subclass: #CMAbstractNode
	instanceVariableNames: 'view viewMetaInfo elementMap sourceMetaInfo childrenNodesFactory adaptor dependents pendingChanges children parent '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMAbstractNode subclass: #CMClassNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMAbstractNode subclass: #CMCollectionNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMCollectionNode subclass: #CMOrderedNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMAbstractNode subclass: #CMObjectNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMObjectNode subclass: #CMRootNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMAbstractNode subclass: #CMTerminalInCollectionNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMAbstractNode subclass: #CMTerminalNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMAbstractNode subclass: #CMTreeNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMTreeNode subclass: #CMTreeHolderNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

CMTreeNode subclass: #CMTreeListNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

SubApplication subclass: #CODE_META_Nodes
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Nodes becomeDefault!

!CMAbstractNode class publicMethodsFor: 'navigation'!

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

	^self modelEditorMETAPerspectivesGeneral, self modelEditorMETAPerspectivesAbstractNode!

modelEditorMETAPerspectivesAbstractNode
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Children'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #('Children')));
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'Parent'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #('Parent')));
		yourself!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #('DescriptionString' 'LongDescriptionString')));
		yourself!

modelEditorMETASelectors

	^self modelEditorMETASelectorsGeneral!

modelEditorMETASelectorsGeneral

	"METAChildSpecAutoViewEditor openOn: CMAbstractNode selector: #modelEditorMETASelectorsGeneral target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 9)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'DescriptionString';
			basicSelector: #descriptionString;
			type: #String;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'DescriptionString';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'LongDescriptionString';
			basicSelector: #longDescriptionString;
			type: #String;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'LongDescriptionString';
			displaySelector: nil;
			canShowInTree: true;
			yourself);
		add: ((Smalltalk at: #METAOrderedCollectionChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Children';
			basicSelector: #children;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Children';
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
			name: 'Parent';
			basicSelector: #parent;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Parent';
			displaySelector: #descriptionString;
			canShowInTree: true;
			objectClassName: #CMAbstractNode;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ViewMetaInfo';
			basicSelector: #viewMetaInfo;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ViewMetaInfo';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEElement;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ElementMap';
			basicSelector: #elementMap;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ElementMap';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEElementMap;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'SourceMetaInfo';
			basicSelector: #sourceMetaInfo;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'SourceMetaInfo';
			displaySelector: #name;
			canShowInTree: true;
			objectClassName: #CODEElement;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'Adaptor';
			basicSelector: #adaptor;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'Adaptor';
			displaySelector: #printString;
			canShowInTree: true;
			objectClassName: #CMAdaptor;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
			yourself);
		add: ((Smalltalk at: #METAClassChildSpec ifAbsent: [ ^#() copy])  new
			name: 'ChildrenNodesFactory';
			basicSelector: #childrenNodesFactory;
			type: #Object;
			displayValue: true;
			isChildren: true;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'ChildrenNodesFactory';
			displaySelector: #printString;
			canShowInTree: true;
			objectClassName: #CMChildrenNodesFactory;
			deletionPolicy: #Default;
			deletionMode: #Default;
			showChildren: true;
			showInEditor: true;
			menuSelector: nil;
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
			showInEditor: true;
			menuSelector: nil;
			yourself);
		yourself!

modelEditorPathSelectors

	^self modelEditorPathSelectorsGeneral!

modelEditorPathSelectorsGeneral

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #('Children' 'Parent' 'ViewMetaInfo' 'SourceMetaInfo' 'ElementMap'  'View' 'Adaptor' 'ChildrenNodesFactory')!

pathSelectors
	^self metaSelectors! !

!CMAbstractNode publicMethodsFor: 'accessing'!

adaptor
	^adaptor!

adaptor: theAdaptor
	adaptor := theAdaptor!

childrenNodesFactory
	^childrenNodesFactory!

childrenNodesFactory: theChildrenNodesFactory
	childrenNodesFactory := theChildrenNodesFactory!

dependents
	^super dependents
	"dependents isNil ifTrue: [ self initDependents].
	^dependents"!

elementMap
	^elementMap!

elementMap: theElementMap
	elementMap := theElementMap!

pendingChanges
	pendingChanges isNil ifTrue: [ self initPendingChanges].
	^pendingChanges!

sourceMetaInfo
	^sourceMetaInfo!

sourceMetaInfo: theMetaInfo
	sourceMetaInfo := theMetaInfo!

view
	^view!

view: theView
	view := theView!

viewMetaInfo
	^viewMetaInfo!

viewMetaInfo: theMetaInfo
	viewMetaInfo := theMetaInfo! !

!CMAbstractNode publicMethodsFor: 'connect'!

connectAdaptor
	| anAdaptor |
	anAdaptor := self adaptor.
	anAdaptor isNil ifTrue: [ ^self].
	
	anAdaptor connectToNode: self!

forzeAllAccessesPending! !

!CMAbstractNode publicMethodsFor: 'derived accessing'!

descriptionString
	| aViewMetaInfoString aSourceMetaInfoString anObjectString |
	aViewMetaInfoString := self viewMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self viewMetaInfo kind , ' ', self viewMetaInfo name].
	aSourceMetaInfoString := self sourceMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self sourceMetaInfo kind , ' ', self sourceMetaInfo name].

	anObjectString := nil.

	anObjectString := self adaptor isNil 
		ifTrue:  [ '?Adpt' copy] 
		ifFalse: [ 
			(Object messageNotUnderstoodSignal handle: [:anEx | anEx returnWith: nil ] do: [
				self adaptor sourceObject 
			]) isNil 
					ifFalse: [ self adaptor sourceObject printString]
					ifTrue: [  
						self parent isNil 
							ifTrue: [ '?Parent' copy] 
							ifFalse: [ 
								(Object messageNotUnderstoodSignal handle: [:anEx | anEx returnWith: nil ] do: [
									self parent adaptor sourceObject
								]) isNil ifTrue: [ 'nil' copy] ifFalse: [ self parent adaptor sourceObject printString]
							]
					]
		].

	^anObjectString , ' : ' , aViewMetaInfoString, ' on ', aSourceMetaInfoString!

longDescriptionString
	| aViewMetaInfoString aSourceMetaInfoString anObjectString |
	aViewMetaInfoString := self viewMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self viewMetaInfo kind , ' ', self viewMetaInfo name].
	aSourceMetaInfoString := self sourceMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self sourceMetaInfo kind , ' ', self sourceMetaInfo name].

	anObjectString := nil.

	anObjectString := self adaptor isNil 
		ifTrue:  [ '?Adpt' copy] 
		ifFalse: [ 
			(Object messageNotUnderstoodSignal handle: [:anEx | anEx returnWith: nil ] do: [
				self adaptor sourceObject 
			]) isNil 
					ifFalse: [ self adaptor sourceObject printString]
					ifTrue: [  
						self parent isNil 
							ifTrue: [ '?Parent' copy] 
							ifFalse: [ 
								(Object messageNotUnderstoodSignal handle: [:anEx | anEx returnWith: nil ] do: [
									self parent adaptor sourceObject
								]) isNil ifTrue: [ 'nil' copy] ifFalse: [ self parent adaptor sourceObject printString]
							]
					]
		].

	^anObjectString , ' : ' , aViewMetaInfoString, ' on ', aSourceMetaInfoString! !

!CMAbstractNode publicMethodsFor: 'initialize-release'!

initDependents
	dependents := OrderedCollection new: 4!

initPendingChanges
	pendingChanges := OrderedCollection new: 4!

recursivelyRemoveChildrenRemovedOnCollection: theCollection

	| someChildren |

	someChildren := children isNil ifTrue: [ OrderedCollection new: 0] ifFalse: [ children copy].

	theCollection addAll:  someChildren.
	someChildren do: [:aNode |
		children remove: aNode.
		aNode forzeParent: nil.
		aNode recursivelyRemoveChildrenRemovedOnCollection: theCollection.
	].
	self release!

recursiveRelease

	children isNil ifFalse: [  children do: [ :aNode |  aNode recursiveRelease]].

	self release!

release

	| anAdaptor |
	anAdaptor := self adaptor.
	anAdaptor isNil ifFalse: [ anAdaptor release].

	view := nil.
	viewMetaInfo := nil.
	elementMap := nil.
	sourceMetaInfo := nil.
	childrenNodesFactory := nil.
	adaptor := nil.
	dependents := nil.
	pendingChanges := nil.
	children := nil.
	parent := nil.

	super release! !

!CMAbstractNode publicMethodsFor: 'navigation'!

browse
	^METAScopedApplicationBrowser
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

!CMAbstractNode publicMethodsFor: 'notification'!

notifyChildrenChanged
	self changed: #children! !

!CMAbstractNode publicMethodsFor: 'printing'!

printOn: theStream
	theStream nextPutAll: self descriptionString! !

!CMAbstractNode publicMethodsFor: 'testing'!

isNilChildren
	^children == nil! !

!CMAbstractNode publicMethodsFor: 'tree'!

children
	children isNil ifTrue: [ self initChildren].
	^children!

existingChildren
	^children!

forzeParent: theParent
	parent := theParent!

initChildren
	| aChildrenNodesFactory someChildren |
	aChildrenNodesFactory := self childrenNodesFactory.
	aChildrenNodesFactory isNil ifTrue: [ ^CMSignals nodeWithoutChildrenNodesFactorySignal raiseWith: self].
 
	someChildren := aChildrenNodesFactory newChildrenNodesForNode: self.

	(someChildren isNil or: [ someChildren isEmpty]) ifTrue: [
		self initEmptyChildren.
		^self
	].

	someChildren do: [:aChild | aChild forzeParent: self].
	someChildren do: [:aChild | aChild connectAdaptor].

	self initEmptyChildren: someChildren size.
	children addAll: someChildren.!

initEmptyChildren
	self initEmptyChildren: 1!

initEmptyChildren: theSize
	children := OrderedCollection new: theSize!

parent
	^parent! !

!CMClassNode publicMethodsFor: 'derived accessing'!

descriptionString
	| aViewMetaInfoName anObjectString anObject |
	aViewMetaInfoName := self viewMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self viewMetaInfo name].

	anObjectString := self adaptor isNil 
		ifTrue:  [ '?Adpt' copy] 
		ifFalse: [ 
			anObject := self adaptor retrieveRelatedObjects.
			anObject isNil  ifTrue: [ 'nil'] ifFalse: [ anObject  printString]].

	aViewMetaInfoName := self viewMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self viewMetaInfo name].

	^'one: ' ,aViewMetaInfoName, ' = ',  anObjectString! !

!CMClassNode publicMethodsFor: 'updating'!

addChildNodeForObj: theObject 

	| aChildrenNodesFactory aNewNode |

	children isNil ifTrue: [ ^self].

	aChildrenNodesFactory := self childrenNodesFactory.
	aChildrenNodesFactory isNil ifTrue: [ CMSignals nodeWithoutChildrenNodesFactorySignal raiseWith: self].

	aNewNode := aChildrenNodesFactory newChildNodeForObject: theObject inNode: self.
	aNewNode isNil ifTrue: [ ^nil].

	children add: aNewNode.
	aNewNode forzeParent: self.
	aNewNode connectAdaptor.!

removeChildNodeWithObj: theObject  removedOnCollection: theRemovedNodesCollection

	| aNodeToRemove  |

	children isNil ifTrue: [ ^nil].

	aNodeToRemove := children detect: [:aNode | aNode adaptor isNil not and: [ aNode adaptor sourceObject == theObject]].
	aNodeToRemove isNil ifTrue: [ ^nil].
	children remove: aNodeToRemove.
	aNodeToRemove forzeParent: nil.
	aNodeToRemove recursivelyRemoveChildrenRemovedOnCollection: theRemovedNodesCollection.
	^aNodeToRemove! !

!CMCollectionNode publicMethodsFor: 'derived accessing'!

descriptionString
	| aNumChildrenString someObjects aViewMetaInfoName |
	aViewMetaInfoName := self viewMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self viewMetaInfo name].

	aNumChildrenString := self adaptor isNil 
		ifTrue:  [ '?Adpt' copy] 
		ifFalse: [ 
			someObjects := self adaptor retrieveRelatedObjects.
			someObjects isNil  ifTrue: [ '?Objs'] ifFalse: [ someObjects size printString]].

	aViewMetaInfoName := self viewMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self viewMetaInfo name].

	^'many: ' ,aViewMetaInfoName, ' (', aNumChildrenString, ')'! !

!CMCollectionNode publicMethodsFor: 'updating'!

addChildNodeForObj: theObject 

	| aChildrenNodesFactory aNewNode |

	children isNil ifTrue: [ ^self].

	aChildrenNodesFactory := self childrenNodesFactory.
	aChildrenNodesFactory isNil ifTrue: [ CMSignals nodeWithoutChildrenNodesFactorySignal raiseWith: self].

	aNewNode := aChildrenNodesFactory newChildNodeForObject: theObject inNode: self.
	aNewNode isNil ifTrue: [ ^nil].

	children add: aNewNode.
	aNewNode forzeParent: self.
	aNewNode connectAdaptor.!

removeChildNodeWithObj: theObject  removedOnCollection: theRemovedNodesCollection

	| aNodeToRemove  |

	children isNil ifTrue: [ ^nil].

	aNodeToRemove := children detect: [:aNode | aNode adaptor isNil not and: [ aNode adaptor sourceObject == theObject]].
	aNodeToRemove isNil ifTrue: [ ^nil].
	children remove: aNodeToRemove.
	aNodeToRemove forzeParent: nil.
	aNodeToRemove recursivelyRemoveChildrenRemovedOnCollection: theRemovedNodesCollection.
	^aNodeToRemove! !

!CMObjectNode publicMethodsFor: 'derived accessing'!

descriptionString
	| anObjectString |

	anObjectString := self adaptor isNil 
		ifTrue:  [ '?Adpt' copy] 
		ifFalse: [ self adaptor sourceObject printString].

	^'obj:' , anObjectString! !

!CMObjectNode publicMethodsFor: 'tree'!

children
	^Object messageNotUnderstoodSignal handle: [:anEx | self halt. anEx reject] do: [super children]! !

!CMTerminalNode class publicMethodsFor: 'navigation'!

modelEditorMETAPerspectives

	^self modelEditorMETAPerspectivesTerminal, super modelEditorMETAPerspectives!

modelEditorMETAPerspectivesGeneral
	^OrderedCollection new
		addLast:  ((Smalltalk at: #METAPerspectiveSpec)  
			name: 'General'
			view: ((Smalltalk at: #METACachedView) new
				metaSelectorsSource:  self; 
				metaSelectorsSelector: #modelEditorMETASelectors;
				pathSelectorsSelector: #modelEditorPathSelectors;
				metaSelectorsToSelect: #('DescriptionString' 'LongDescriptionString' 'SourceValuePrintString')));
		yourself!

modelEditorMETAPerspectivesTerminal

	^OrderedCollection new
		yourself!

modelEditorMETASelectors

	^super modelEditorMETASelectors , self modelEditorMETASelectorsTerminal!

modelEditorMETASelectorsTerminal

	"METAChildSpecAutoViewEditor openOn: CODETerminal selector: #modelEditorMETASelectorsTerminal target: nil selector: nil."

	self ojoMETASelectors.

	^(OrderedCollection new: 5)
		add: ((Smalltalk at: #METATerminalChildSpec ifAbsent: [ ^#() copy])  new
			name: 'SourceValuePrintString';
			basicSelector: #sourceValuePrintString;
			type: #String;
			displayValue: true;
			isChildren: false;
			isStatic: true;
			creationPolicy: #Default;
			creationMode: #Create;
			helpString: 'SourceValuePrintString';
			displaySelector: nil;
			yourself);

		yourself!

modelEditorPathSelectors

	| aLocalView |
	aLocalView := self modelEditorPathSelectorsTerminal.

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: aLocalView metaSelectorsSelector;
		pathSelectorsSelector: aLocalView pathSelectorsSelector;
		metaSelectorsToSelect: aLocalView metaSelectorsToSelect, super modelEditorPathSelectors metaSelectorsToSelect!

modelEditorPathSelectorsTerminal

	^METACachedView new
		metaSelectorsSource:  self;
		metaSelectorsSelector: #modelEditorMETASelectors;
		pathSelectorsSelector: #modelEditorPathSelectors;
		metaSelectorsToSelect: #()! !

!CMTerminalNode publicMethodsFor: 'derived accessing'!

descriptionString
	| aViewMetaInfoName aValueString |

	aViewMetaInfoName := self viewMetaInfo isNil ifTrue: [ '?Vmi'] ifFalse: [ self viewMetaInfo name].

	aValueString := self sourceValuePrintString.
	^'attr: ' ,aViewMetaInfoName, ' = ', aValueString! !

!CMTerminalNode publicMethodsFor: 'value'!

sourceValuePrintString
	| anAdaptor aValue |
	anAdaptor := self adaptor.
	anAdaptor isNil ifTrue: [ ^'?Adpt' copy].

	aValue := anAdaptor sourceValue.
	^aValue printString! !

CMAbstractNode initializeAfterLoad!
CMClassNode initializeAfterLoad!
CMCollectionNode initializeAfterLoad!
CMOrderedNode initializeAfterLoad!
CMObjectNode initializeAfterLoad!
CMRootNode initializeAfterLoad!
CMTerminalInCollectionNode initializeAfterLoad!
CMTerminalNode initializeAfterLoad!
CMTreeNode initializeAfterLoad!
CMTreeHolderNode initializeAfterLoad!
CMTreeListNode initializeAfterLoad!
CODE_META_Nodes initializeAfterLoad!

CODE_META_Nodes loaded!
