'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Constructors in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Constructors becomeDefault!

Object subclass: #CMConstructor
	instanceVariableNames: 'context reader '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

Object subclass: #CMConstructorContext
	instanceVariableNames: 'stackFrames model rootInstance '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

Object subclass: #CMConstructorContextFrame
	instanceVariableNames: 'context element metaInfo '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

CMConstructorContextFrame subclass: #CMObjectConstructorContextFrame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

CMObjectConstructorContextFrame subclass: #CMRootObjectConstructorContextFrame
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

Object subclass: #CMScanner
	instanceVariableNames: 'stream context observers '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

Object subclass: #CMScannerContext
	instanceVariableNames: 'stackFrames '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

Object subclass: #CMScannerContextFrame
	instanceVariableNames: 'elementTag context scannerState '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

SubApplication subclass: #CODE_META_Constructors
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Constructors becomeDefault!

!CMConstructor class publicMethodsFor: 'constructing'!

instantiateModel: theModel fromXMLString: theXMLString rootInstance: theRootInstance
	"CMConstructor construct: EDOCSimpleMetaInfoHolder  currentModel  fromXMLString: MDAMEmpresa empresaEmeCeroSystemsWeb01XMLstringFromUTF8File"

	| aConstructor aStream aReader aConstructorContext aScannerContext aRootInstance |

	theModel isNil ifTrue: [ ^nil].
	(theXMLString isNil or: [ theXMLString isEmpty]) ifTrue: [ ^nil].
	theRootInstance isNil ifTrue: [ ^nil].

	aConstructor := self new.

	aConstructorContext := CMConstructorContext withModel: theModel rootInstance: theRootInstance.
	aConstructor  withContext: aConstructorContext.

	aScannerContext := CMScannerContext new.
	aStream := ReadStream on: theXMLString.

	aReader := CMScanner new.
	aReader fromStream: aStream.
	aReader withContext: aScannerContext.

	aConstructor withReader: aReader.

	aReader scan.

	aRootInstance := aConstructorContext rootInstance.
	^aRootInstance! !

!CMConstructor class publicMethodsFor: 'debug'!

trace
	^false! !

!CMConstructor class publicMethodsFor: 'examples'!

empresaEmeCeroSystemsWeb01ConstructFromXMLString: theXMLString
	"CMConstructor empresaEmeCeroSystemsWeb01ConstructFromXMLString: MDAMEmpresa empresaEmeCeroSystemsWeb01XMLstring"

	| aConstructor aStream aReader aConstructorContext aScannerContext someRoots |

	(theXMLString isNil or: [ theXMLString isEmpty]) ifTrue: [ ^nil].

	aConstructor := self new.

	aConstructorContext := CMConstructorContext withAcceptedClasses: (Array with: MDAMEmpresa with: MDAMCategoria with: MDAMClase with: MDAMItem).
	aConstructor  withContext: aConstructorContext.

	aScannerContext := CMScannerContext new.
	aStream := ReadStream on: theXMLString.

	aReader := CMScanner new.
	aReader fromStream: aStream.
	aReader withContext: aScannerContext.

	aConstructor withReader: aReader.

	aReader scan.

	someRoots := aConstructorContext roots.
	someRoots first browsePath.
	^someRoots!

empresaEmeCeroSystemsWeb01ReadXMLString
	"CMReaderConstructor empresaEmeCeroSystemsWeb01ReadXMLString"

	| aXMLString |

	aXMLString := MDAMEmpresa empresaEmeCeroSystemsWeb01XMLstring.
	aXMLString isNil ifTrue: [ self halt: 'aXMLString isNil'].

	^CMReaderConstructor exampleReadXMLString01: aXMLString!

exampleReadXMLString01: theXMLString
	"CMConstructor exampleReadXMLString01: CMConstructor exampleXMLStrings01 first"
	"CMConstructor exampleReadXMLString01: '<E1><E2/></E1>'"

	| aConstructor aContext aStream aReader aFrame |

	(theXMLString isNil or: [ theXMLString isEmpty]) ifTrue: [ ^nil].

	aConstructor := self new.

	aContext := CMScannerContext new.

	aStream := ReadStream on: theXMLString.

	aReader := CMScanner new.
	aReader fromStream: aStream.
	aReader withContext: aContext.
	aReader observersAdd: aConstructor.

	aConstructor withReader: aReader.

	aReader scan.

	aFrame := aContext frame.
	aFrame isNil ifTrue: [ ^nil].
	^aFrame!

exampleReadXMLStrings01
	"CMConstructor exampleReadXMLStrings01"

	| someXMLStrings |
	someXMLStrings := CMConstructor exampleXMLStrings01.
	(someXMLStrings isNil or: [ someXMLStrings isEmpty]) ifTrue: [ ^nil].

	someXMLStrings do: [:aXMLString |
		CMConstructor exampleReadXMLString01: aXMLString
	]!

exampleReadXMLStrings02
	"CMConstructor exampleReadXMLStrings02"

	| someXMLStrings |
	someXMLStrings := CMConstructor exampleXMLStrings02.
	(someXMLStrings isNil or: [ someXMLStrings isEmpty]) ifTrue: [ ^nil].

	someXMLStrings do: [:aXMLString |
		CMConstructor exampleReadXMLString01: aXMLString
	]!

exampleXMLStrings01
	"CMConstructor exampleXMLStrings01"

	^#(
	"	''
		' '
		' a '"
		'<E1/>'
		'<E1></E1>'
		'<E1>a</E1>'
		'<E1>aa aa aa</E1>'
		'<E1><E2/></E1>'
		'<E1>aa<E2/>bb</E1>'
		'<E1>aa<E2></E2>aa</E1>'
		'<E1>aa<E2>cc</E2>bb</E1>'
		'<E1>aa<E2>cc<E3></E3>dd</E2>bb</E1>'
		'<E1>aa<E2>cc<E3>ee</E3>dd</E2>bb</E1>'
	)!

exampleXMLStrings02
	"CMReaderConstructor exampleXMLStrings02"

	^#(
		'<E1 at1="vat1"/>'
		'<E1 at1="vat1"></E1>'
		'<E1 at1="vat1">aa aa aa</E1>'
		'<E1 at1="vat1" at2="vat2">aa aa aa</E1>'
		'<E1 at1="vat1"><E2 at2="vat2"/></E1>'
	)! !

!CMConstructor class publicMethodsFor: 'visit-refine'!

tagNameSelector
	^#tagName! !

!CMConstructor publicMethodsFor: 'accessing'!

context
	^context!

reader
	^reader! !

!CMConstructor publicMethodsFor: 'debug'!

error: theErrorMessage

	| aMethodName aMethodKind |
	aMethodName := thisContext sender method selector.
	aMethodKind := aMethodName copyUpTo: $:.
	aMethodKind isEmpty ifTrue: [ aMethodKind := aMethodName].

	^super error: aMethodKind , ' : ' , theErrorMessage!

log
	| aMethodName aMethodKind  |
	self trace ifFalse: [ ^self].

	aMethodName := thisContext sender method selector.
	aMethodKind := aMethodName copyUpTo: $:.
	aMethodKind isEmpty ifTrue: [ aMethodKind := aMethodName].

	Transcript show: aMethodKind; cr!

trace
	^self class trace! !

!CMConstructor publicMethodsFor: 'events'!

eventAttribute: theAttributeName value: theAttributeValue

	| aContext |

	(theAttributeName isNil or: [ theAttributeName isEmpty]) ifTrue: [ ^nil].
	aContext := self context.
	aContext isNil ifTrue: [ ^nil].

	^aContext constructAttribute: theAttributeName value: theAttributeValue!

eventElementBegin: theElementTag
	| aContext |

	(theElementTag isNil or: [ theElementTag isEmpty]) ifTrue: [ ^nil].
	aContext := self context.
	aContext isNil ifTrue: [ ^nil].

	^aContext constructElement: theElementTag!

eventElementBeginClose: theElementTag!

eventElementClose: theElementTag
	| aContext |


	(theElementTag isNil or: [ theElementTag isEmpty]) ifTrue: [ ^nil].
	aContext := self context.
	aContext isNil ifTrue: [ ^nil].

	^aContext completeElement: theElementTag!

eventElementClosend: theElementTag
	| aContext |


	(theElementTag isNil or: [ theElementTag isEmpty]) ifTrue: [ ^nil].
	aContext := self context.
	aContext isNil ifTrue: [ ^nil].

	^aContext completeElement: theElementTag!

eventElementHeaderBegin: theElementTag!

eventElementHeaderClosend: theElementTag!

eventElementTextContents: theText

	| aTrimmedText |
	(theText isNil or: [ theText isEmpty]) ifTrue: [ ^nil].

	aTrimmedText := theText trimSeparators.
	aTrimmedText isEmpty ifTrue: [ ^nil].! !

!CMConstructor publicMethodsFor: 'setup'!

withContext: theContext
	context := theContext!

withReader: theReader
	reader := theReader.
	reader isNil ifFalse: [ reader observersAdd: self]! !

!CMConstructorContext class publicMethodsFor: 'instance creation'!

withModel: theModel rootInstance: theRootInstance
	| aContext |
	aContext := self new.
	aContext withModel: theModel rootInstance: theRootInstance.
	^aContext! !

!CMConstructorContext class publicMethodsFor: 'preferences'!

preferredContextFrameClass
	^CMConstructorContextFrame!

preferredObjectContextFrameClass
	^CMObjectReaderContextFrame!

preferredTraversalContextFrameClass
	^CMTraversalReaderContextFrame! !

!CMConstructorContext publicMethodsFor: 'accessing'!

model
	^model!

rootInstance
	^rootInstance!

stackFrames
	stackFrames isNil ifTrue: [ self initStackFrames].
	^stackFrames! !

!CMConstructorContext publicMethodsFor: 'constructing'!

completeElement: theElementTag
	| aFrame |
	aFrame := self frame.
	^aFrame isNil 
		ifTrue: [ nil]
		ifFalse: [ aFrame completeElement: theElementTag]!

constructAttribute: theAttributeName value: theAttributeValue
	| aFrame |
	aFrame := self frame.
	^aFrame isNil 
		ifTrue: [ nil]
		ifFalse: [ aFrame constructAttribute: theAttributeName value: theAttributeValue]!

constructElement: theElementTag
	| aFrame |
	aFrame := self frame.
	^aFrame isNil 
		ifTrue: [ self constructRootElement: theElementTag]
		ifFalse: [ aFrame constructSubElement: theElementTag]!

constructRootElement: theElementTag
	| aNewElement aNewFrame aFrame aRootInstance aRootType aFeature aReferencedType |

	(theElementTag isNil or: [ theElementTag isEmpty]) ifTrue: [ ^nil].

	aFrame := self frame.
	aFrame isNil ifFalse: [ ^nil].

	aRootInstance := self rootInstance.
	aRootInstance isNil ifTrue: [ ^nil].
	
	aRootType := aRootInstance metaInfo.
	aRootType isNil ifTrue: [ ^nil].

	aFeature := aRootType effectiveFeatureTypedNamed: theElementTag.
	aFeature isNil ifTrue: [ ^nil].

	aReferencedType := aFeature referencedType.
	aReferencedType isNil ifTrue: [ ^nil].

	aNewElement := aReferencedType createObject.
	aNewElement isNil ifTrue: [ ^nil].

	aFeature isMultiplicityOne
		ifTrue: [ aFeature object: aRootInstance setTC: aNewElement]
		ifFalse: [ aFeature object: aRootInstance addTC: aNewElement].

	aNewFrame := CMRootObjectConstructorContextFrame  forNewElement: aNewElement ofType: aReferencedType inContext: self.
	self pushFrame: aNewFrame.

	^aNewFrame!

pushFrame: theFrame

	| someStackFrames |
	theFrame isNil ifTrue: [ ^nil].

	someStackFrames := self stackFrames.
	someStackFrames isNil ifTrue: [ ^nil].

	someStackFrames addLast: theFrame.

	^theFrame! !

!CMConstructorContext publicMethodsFor: 'initialize-release'!

initStackFrames
	stackFrames := OrderedCollection new: 64!

release

	| someStackFrames |
	someStackFrames := stackFrames copy.
	stackFrames := nil.

	someStackFrames do: [:aStackFrame | aStackFrame release].! !

!CMConstructorContext publicMethodsFor: 'preferences'!

preferredObjectContextFrameClass
	^self class preferredObjectContextFrameClass!

preferredTraversalContextFrameClass
	^self class preferredTraversalContextFrameClass! !

!CMConstructorContext publicMethodsFor: 'setup'!

withModel: theModel rootInstance: theRootInstance

	model := theModel.
	rootInstance := theRootInstance! !

!CMConstructorContext publicMethodsFor: 'stack'!

depth

	| someStackFrames |

	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^0].

	^someStackFrames size!

depthAt: theFrame

	| someStackFrames aFrame anIndex |
	theFrame isNil ifTrue: [ ^0].

	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^0].

	aFrame := someStackFrames last.
	aFrame == theFrame ifTrue: [ ^someStackFrames size].

	anIndex := someStackFrames indexOf: theFrame.
	^anIndex!

frame
	| someStackFrames aFrame |
	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^nil].

	aFrame := someStackFrames last.
	^aFrame!

frameBefore: theFrame

	| aFrame someStackFrames anIndex |

	theFrame isNil ifTrue: [ ^self].

	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^self].

	anIndex := someStackFrames indexOf: theFrame.
	anIndex < 2 ifTrue: [ ^nil].

	aFrame := someStackFrames at: anIndex - 1.
	^aFrame!

popFromStack: theFrame

	| aFrame someStackFrames |

	theFrame isNil ifTrue: [ ^self].

	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^self].

	aFrame := someStackFrames last.
	aFrame == theFrame ifFalse: [ ^self].

	someStackFrames removeLast.! !

!CMConstructorContextFrame class publicMethodsFor: 'instance creation'!

inContext: theContext 

	| aFrame |
	aFrame := self new.
	aFrame inContext: theContext.

	^aFrame! !

!CMConstructorContextFrame publicMethodsFor: 'accessing'!

context
	^context!

element
	^element!

metaInfo
	^metaInfo! !

!CMConstructorContextFrame publicMethodsFor: 'initialize-release'!

release
	context := nil.
	element := nil.
	elementTagMap := nil! !

!CMConstructorContextFrame publicMethodsFor: 'setup'!

forNewElement: theElement
	element := theElement!

inContext: theContext
	context := theContext!

metaInfo: theMetaInfo
	metaInfo := theMetaInfo! !

!CMConstructorContextFrame publicMethodsFor: 'stack'!

depth
	| aContext |
	aContext := self context.
	aContext isNil ifTrue: [ ^0].

	^aContext depthAt: self!

popFromStack
	| aContext |
	aContext := self context.
	aContext isNil ifTrue: [ ^self].
	
	aContext popFromStack: self.
	self release! !

!CMObjectConstructorContextFrame class publicMethodsFor: 'instance creation'!

forNewElement: theNewElement ofType: theType  inContext: theContext

	| aFrame |

	aFrame := super inContext: theContext.
	aFrame metaInfo: theType.
	aFrame forNewElement: theNewElement.

	^aFrame! !

!CMObjectConstructorContextFrame publicMethodsFor: 'constructing'!

completeElement: theElementTag

	| aContext |
	aContext := self context.
	aContext isNil ifTrue: [ ^nil].

	aContext popFromStack: aContext frame!

constructAttribute: theAttributeName value: theAttributeValue

	| anInstance aType anAttribute |
	(theAttributeName isNil or: [ theAttributeValue isEmpty]) ifTrue: [ ^nil].

	anInstance := self element.
	anInstance isNil ifTrue: [ ^nil].
	
	aType := anInstance metaInfo.
	aType isNil ifTrue: [ ^nil].

	anAttribute := aType effectiveAttributeNamed: theAttributeName.
	anAttribute isNil ifTrue: [ ^nil].

	anAttribute isMultiplicityOne
		ifTrue: [ anAttribute object: anInstance setTC: theAttributeValue]
		ifFalse: [ anAttribute object: anInstance addTC: theAttributeValue].

	^self!

constructSubElement: theElementTag

	| anInstance aType aFeature aReferencedType aNewElement aNewFrame aContext |
	(theElementTag isNil or: [ theElementTag isEmpty]) ifTrue: [ ^nil].

	aContext := self context.
	aContext isNil ifTrue: [ ^nil].

	anInstance := self element.
	anInstance isNil ifTrue: [ ^nil].
	
	aType := anInstance metaInfo.
	aType isNil ifTrue: [ ^nil].

	aFeature := aType effectiveFeatureTypedNamed: theElementTag.
	aFeature isNil ifTrue: [ ^nil].

	aReferencedType := aFeature referencedType.
	aReferencedType isNil ifTrue: [ ^nil].

	aNewElement := aReferencedType createObject.
	aNewElement isNil ifTrue: [ ^nil].

	aFeature isMultiplicityOne
		ifTrue: [ aFeature object: anInstance setTC: aNewElement]
		ifFalse: [ aFeature object: anInstance addTC: aNewElement].

	aNewFrame := CMObjectConstructorContextFrame  forNewElement: aNewElement ofType: aReferencedType inContext: aContext.
	aContext pushFrame: aNewFrame.

	^aNewFrame! !

!CMScanner class publicMethodsFor: 'debug'!

logEvents
	^false!

trace
	^false! !

!CMScanner publicMethodsFor: 'accessing'!

context
	^context!

observers
	observers isNil ifTrue: [ self observersInitialize].
	^observers copy!

stream
	^stream! !

!CMScanner publicMethodsFor: 'debug'!

log
	|  aMethodName aMethodKind  |
	self trace ifFalse: [ ^self].

	aMethodName := thisContext sender method selector.
	aMethodKind := aMethodName copyUpTo: $:.
	aMethodKind isEmpty ifTrue: [ aMethodKind := aMethodName].

	Transcript show: aMethodKind; cr!

logEvents
	^self class logEvents!

logState: theScannerState
	|  |
	self trace ifFalse: [ ^self].

	self trace ifFalse: [ ^self].

	Transcript show: theScannerState; cr!

trace
	^self class trace! !

!CMScanner publicMethodsFor: 'initialize-release'!

observersInitialize
	observers := OrderedCollection new: 8! !

!CMScanner publicMethodsFor: 'notifying'!

notifyAttribute: theAttributeName value: theAttributeValue

	| someObservers |

	self logEvents ifTrue: [ Transcript space; show: theAttributeName ; show: '="';  show: theAttributeValue; show: '"'; space].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver |
		anObserver eventAttribute: theAttributeName value: theAttributeValue
	]!

notifyElementBegin: theElementTag

	| someObservers |
	self logEvents ifTrue: [	Transcript tab; show: '<'; show: theElementTag; space; flush].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver |
		anObserver eventElementBegin: theElementTag
	]!

notifyElementBeginClose: theElementTag

	| someObservers |

	self logEvents ifTrue: [ Transcript space; show: '>'; cr].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver | 
		anObserver eventElementBeginClose: theElementTag
	]!

notifyElementClose: theElementTag

	| someObservers |

	self logEvents ifTrue: [ Transcript tab; show: '</'; show: theElementTag; show: '>'; cr].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver |
		anObserver eventElementClose: theElementTag
	]!

notifyElementClosend: theElementTag

	| someObservers |

	self logEvents ifTrue: [ Transcript show: ' />'; cr].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver |
		anObserver eventElementClosend: theElementTag
	]!

notifyElementHeaderBegin: theElementTag

	| someObservers |
	self logEvents ifTrue: [	Transcript tab; show: '<?'; show: theElementTag; space; flush].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver |
		anObserver eventElementHeaderBegin: theElementTag
	]!

notifyElementHeaderClosend: theElementTag

	| someObservers |

	self logEvents ifTrue: [ Transcript show: ' ?>'; cr].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver |
		anObserver eventElementHeaderClosend: theElementTag
	]!

notifyElementTextContents: theText

	| someObservers |
	theText isNil ifTrue: [ ^self].
theText = '>' ifTrue: [ self halt].

	self logEvents ifTrue: [ Transcript show: theText; cr].

	someObservers := self observers.
	(someObservers isNil or: [ someObservers isEmpty]) ifTrue: [ ^self].

	someObservers do: [:anObserver |
		anObserver eventElementTextContents: theText
	]! !

!CMScanner publicMethodsFor: 'scanning'!

decodeEntitiesIn: theString
	| aMapping aStream aWriteStream aChunk aEntityName aEntityExpansion anEntityKey |
	aMapping := METAStoreWriterXML reverseMinimalCharacterMapping.
	(aMapping isNil or: [ aMapping isEmpty]) ifTrue: [ ^theString].

	aStream := theString readStream.
	aWriteStream := WriteStream on: (String new: theString size + 32).

	[
		aStream atEnd ifTrue: [ ^aWriteStream contents].
		aChunk := aStream upTo: $&.
		aWriteStream nextPutAll: aChunk.
		aStream atEnd ifTrue: [ ^aWriteStream contents].
		aEntityName := aStream upTo: $;.
		aEntityName isEmpty 
			ifTrue: [    aWriteStream nextPutAll: '&;']
			ifFalse: [ 
				anEntityKey := '&', aEntityName ,';'.
				aEntityExpansion := aMapping at: anEntityKey ifAbsent: [ nil].
				aEntityExpansion isNil
					ifTrue: [  aWriteStream nextPutAll: anEntityKey]
					ifFalse: [ aWriteStream nextPut: aEntityExpansion]
			]
	] repeat.!

nextAttributeNameFrom: theStream
	| aWriteStream aC |
	theStream atEnd ifTrue: [ ^nil].

	theStream skipSeparators.

	aWriteStream := WriteStream on: (String new: 256).
	
	[ 
		theStream atEnd
	]
		whileFalse: 
	[
		aC := theStream peek.
		(aC isSeparator or: [ aC = $" or: [ aC = $/ or: [ aC = $> or: [ aC = $< or: [ aC = $!! or: [aC = $& or: [ aC = $; or: [ aC = $= ]]]]]]]]) ifTrue: [ ^aWriteStream contents].
		aWriteStream nextPut: aC.
		theStream next
	].
	^aWriteStream contents!

nextAttributeValueFrom: theStream
	| anAttributeValue |
	theStream atEnd ifTrue: [ ^nil].

	anAttributeValue := theStream upTo: $".
	^anAttributeValue!

nextElementTagFrom: theStream
	| aWriteStream aC |
	theStream atEnd ifTrue: [ ^nil].

	theStream skipSeparators.

	aWriteStream := WriteStream on: (String new: 256).
	
	[ 
		theStream atEnd
	]
		whileFalse: 
	[
		aC := theStream peek.
		(aC isSeparator or: [ aC = $" or: [ aC = $/ or: [ aC = $> or: [ aC = $< or: [ aC = $!! or: [aC = $& or: [ aC = $; or: [ aC = $= ]]]]]]]]) ifTrue: [ ^aWriteStream contents].
		aWriteStream nextPut: aC.
		theStream next
	].
	^aWriteStream contents!

scan
	| aContext aContextFrame aScannerState aStream anElementTag anElementTextContents aDecodedElementTextContents aContextFrameElementTag aContents anAttributeName anAttributeValue |

(self logEvents  or: [ self trace]) ifTrue: [ Transcript cr; cr; cr].

	aContext := self context.
	aContext isNil ifTrue: [ ^nil].
	
	aStream := self stream.
	aStream isNil ifTrue: [ ^nil].
	
	
self logEvents ifTrue: [ aContents := aStream contents. Transcript cr; cr; show: 'Scanning:'; show: aContents; cr. aStream reset].


	[aStream atEnd]
		whileFalse: 	
	[ 
		aContextFrame := aContext frame.
		aScannerState := aContextFrame isNil 
			ifTrue: [ #expectElemenHeaderOrRootElementBeginOpen]
			ifFalse: [ aContextFrame scannerState].
	
		self logState: aScannerState.

		aScannerState = #expectElemenHeaderOrRootElementBeginOpen ifTrue: [    
			aStream upTo: $<.
			aStream atEnd ifTrue: [ ^self].
			aStream peek = $?
				ifTrue: [ 
					aStream next.
					aStream atEnd ifTrue: [ ^self].
					anElementTag := self nextElementTagFrom: aStream.
					anElementTag isEmpty ifTrue: [ ^self].
					aContextFrame := aContext pushElementTag: anElementTag scannerState: #expectAttributeNameBeginOrElementHeaderClosend.
					self notifyElementHeaderBegin: anElementTag
				]
				ifFalse: [ 
					anElementTag := self nextElementTagFrom: aStream.
					anElementTag isEmpty ifTrue: [ ^self].
					aContextFrame := aContext pushElementTag: anElementTag scannerState: #expectAttributeNameBeginOrElementCloseOrElementClosend.
					self notifyElementBegin: anElementTag
				]
		] ifFalse: [ 

		aScannerState = #expectRootElementBeginOpen ifTrue: [    
			aStream upTo: $<.
			aStream atEnd ifTrue: [ ^self].
			anElementTag := self nextElementTagFrom: aStream.
			anElementTag isEmpty ifTrue: [ ^self].
			aContextFrame := aContext pushElementTag: anElementTag scannerState: #expectAttributeNameBeginOrElementCloseOrElementClosend.
			self notifyElementBegin: anElementTag

		] ifFalse: [ aScannerState = #expectElementBeginOpenOrEndOpen ifTrue: [    
			anElementTextContents := aStream upTo: $<.
			anElementTextContents isEmpty ifFalse: [ 
				aDecodedElementTextContents := METAStoreWriterXML decodeEntitiesIn: anElementTextContents.
				aDecodedElementTextContents isEmpty ifFalse: [ 
					aDecodedElementTextContents := aDecodedElementTextContents trimSeparators.
					aDecodedElementTextContents isEmpty ifFalse: [ 
						self notifyElementTextContents: aDecodedElementTextContents
					]
				]
			].
			aStream atEnd ifTrue: [ ^self].

			aStream peek = $/ 
				ifTrue: [ 
					aStream next.
					aStream atEnd ifTrue: [ ^self].

					anElementTag := self nextElementTagFrom: aStream.
					anElementTag isEmpty ifTrue: [ ^self].
					aContextFrame := aContext frame.
					aContextFrameElementTag := aContextFrame elementTag.
					anElementTag = aContextFrameElementTag ifFalse: [^self].
					aStream atEnd ifTrue: [ ^false].
					aStream skipSeparators.
					aStream atEnd ifTrue: [ ^false].
					aStream peek = $> ifFalse: [ ^self].
					aStream next.
					self notifyElementClose: anElementTag.

					aContext popFromStack: aContext frame.
					aContextFrame := aContext frame.
					aContextFrame isNil ifFalse: [
						aContextFrame scannerState: #expectElementBeginOpenOrEndOpen
					]
				]
				ifFalse: [ 
					anElementTag := self nextElementTagFrom: aStream.
					anElementTag isEmpty ifTrue: [ ^self].
					aContextFrame := aContext pushElementTag: anElementTag scannerState: #expectAttributeNameBeginOrElementCloseOrElementClosend.
					self notifyElementBegin: anElementTag
				]
		] ifFalse: [ aScannerState = #expectAttributeNameBeginOrElementCloseOrElementClosend ifTrue: [ 
			aStream skipSeparators.
			aStream atEnd ifTrue: [ ^self].
			aStream peek = $> ifTrue: [
				aStream next.
				self notifyElementBeginClose: aContext frame elementTag.
				aContextFrame := aContext frame.
				aContextFrame isNil ifFalse: [
					aContextFrame scannerState: #expectElementBeginOpenOrEndOpen
				]
			] ifFalse: [
			aStream peek = $/ ifTrue: [
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				aStream peek = $> ifFalse: [ ^self].
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				self notifyElementClosend: aContext frame elementTag.
				aContext popFromStack: aContext frame.
				aContextFrame := aContext frame.
				aContextFrame isNil ifFalse: [
					aContextFrame scannerState: #expectElementBeginOpenOrEndOpen
				]
			] ifFalse: [ 
				anAttributeName := self nextAttributeNameFrom: aStream.
				anAttributeName isEmpty ifTrue: [ ^self].
				aStream atEnd ifTrue: [ ^self].
				aStream skipSeparators.
				aStream atEnd ifTrue: [ ^self].
				aStream peek = $= ifFalse: [ ^self].
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				aStream skipSeparators.
				aStream atEnd ifTrue: [ ^self].
				aStream peek = $" ifFalse: [ ^self].
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				anAttributeValue := self nextAttributeValueFrom: aStream.
				aStream atEnd ifTrue: [ ^self].
				self notifyAttribute: anAttributeName value: anAttributeValue.
			]]

		] ifFalse: [ aScannerState = #expectAttributeNameBeginOrElementHeaderClosend ifTrue: [ 
			aStream skipSeparators.
			aStream atEnd ifTrue: [ ^self].
			aStream peek = $> ifTrue: [
				^self error: 'element header close has the form ?>'
			] ifFalse: [
			aStream peek = $? ifTrue: [
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				aStream peek = $> ifFalse: [ ^self].
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				self notifyElementHeaderClosend: aContext frame elementTag.
				aContext popFromStack: aContext frame.
				aContextFrame := aContext frame.
				aContextFrame isNil ifFalse: [
					aContextFrame scannerState: #expectElementBeginOpenOrEndOpen
				]
			] ifFalse: [ 
				anAttributeName := self nextAttributeNameFrom: aStream.
				anAttributeName isEmpty ifTrue: [ ^self].
				aStream atEnd ifTrue: [ ^self].
				aStream skipSeparators.
				aStream atEnd ifTrue: [ ^self].
				aStream peek = $= ifFalse: [ ^self].
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				aStream skipSeparators.
				aStream atEnd ifTrue: [ ^self].
				aStream peek = $" ifFalse: [ ^self].
				aStream next.
				aStream atEnd ifTrue: [ ^self].
				anAttributeValue := self nextAttributeValueFrom: aStream.
				aStream atEnd ifTrue: [ ^self].
				self notifyAttribute: anAttributeName value: anAttributeValue.
			]]

		] ifFalse: [

		]]]]]]! !

!CMScanner publicMethodsFor: 'setup'!

fromStream: theStream
	stream := theStream!

observersAdd: theObserver

	theObserver isNil ifTrue: [ ^self].
	observers isNil 
		ifTrue: [ observers := OrderedCollection new: 8]
		ifFalse: [ observers isEmpty ifFalse: [ (observers includes: theObserver) ifTrue: [ ^self]]].

	observers add: theObserver!

withContext: theContext
	context := theContext! !

!CMScannerContext class publicMethodsFor: 'preferences'!

preferredContextFrameClass
	^CMScannerContextFrame!

preferredObjectContextFrameClass
	^CMObjectScannerContextFrame!

preferredTraversalContextFrameClass
	^CMTraversalScannerContextFrame! !

!CMScannerContext publicMethodsFor: 'accessing'!

stackFrames
	stackFrames isNil ifTrue: [ self initStackFrames].
	^stackFrames! !

!CMScannerContext publicMethodsFor: 'initialize-release'!

initStackFrames
	stackFrames := OrderedCollection new: 64!

release

	| someStackFrames |
	someStackFrames := stackFrames copy.
	stackFrames := nil.

	someStackFrames do: [:aStackFrame | aStackFrame release].! !

!CMScannerContext publicMethodsFor: 'stack'!

depth

	| someStackFrames |

	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^0].

	^someStackFrames size!

depthAt: theFrame

	| someStackFrames aFrame anIndex |
	theFrame isNil ifTrue: [ ^0].

	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^0].

	aFrame := someStackFrames last.
	aFrame == theFrame ifTrue: [ ^someStackFrames size].

	anIndex := someStackFrames indexOf: theFrame.
	^anIndex!

frame
	| someStackFrames aFrame |
	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^nil].

	aFrame := someStackFrames last.
	^aFrame!

popFromStack: theFrame

	| aFrame someStackFrames |

	theFrame isNil ifTrue: [ ^self].

	someStackFrames := self stackFrames.
	(someStackFrames isNil or: [ someStackFrames isEmpty]) ifTrue: [ ^self].

	aFrame := someStackFrames last.
	aFrame == theFrame ifFalse: [ ^self].

	someStackFrames removeLast.!

pushElementTag: theElementTag scannerState: theScannerState

	| aFrame someStackFrames |
	theElementTag isNil ifTrue: [ ^nil].

	someStackFrames := self stackFrames.
	someStackFrames isNil ifTrue: [ ^nil].

	aFrame := CMScannerContextFrame forElementTag: theElementTag scannerState: theScannerState inContext: self.

	someStackFrames add: aFrame.

	^aFrame! !

!CMScannerContextFrame class publicMethodsFor: 'instance creation'!

forElementTag: theElementTag scannerState: theScannerState inContext: theContext
	
	| aContextFrame |
	aContextFrame := self new.
	aContextFrame forElementTag: theElementTag.
	aContextFrame inContext: theContext.
	aContextFrame scannerState: theScannerState.
	^aContextFrame! !

!CMScannerContextFrame publicMethodsFor: 'accessing'!

context
	^context!

elementTag
	^elementTag!

scannerState
	^scannerState! !

!CMScannerContextFrame publicMethodsFor: 'initialize-release'!

release
	elementTag := nil.
	context := nil.
	scannerState := nil.! !

!CMScannerContextFrame publicMethodsFor: 'setup'!

forElementTag: theElementTag
	elementTag := theElementTag!

inContext: theContext
	context := theContext!

scannerState: theScannerState
	theScannerState isNil ifTrue: [ ^self].

	scannerState := theScannerState! !

!CMScannerContextFrame publicMethodsFor: 'stack'!

depth
	| aContext |
	aContext := self context.
	aContext isNil ifTrue: [ ^0].

	^aContext depthAt: self!

popFromStack
	| aContext |
	aContext := self context.
	aContext isNil ifTrue: [ ^self].
	
	aContext popFromStack: self.
	self release! !

CMConstructor initializeAfterLoad!
CMConstructorContext initializeAfterLoad!
CMConstructorContextFrame initializeAfterLoad!
CMObjectConstructorContextFrame initializeAfterLoad!
CMRootObjectConstructorContextFrame initializeAfterLoad!
CMScanner initializeAfterLoad!
CMScannerContext initializeAfterLoad!
CMScannerContextFrame initializeAfterLoad!
CODE_META_Constructors initializeAfterLoad!

CODE_META_Constructors loaded!
