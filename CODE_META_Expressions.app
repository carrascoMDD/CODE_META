'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Expressions in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Expressions becomeDefault!

Object subclass: #CMExpressionNode
	instanceVariableNames: 'kind contextTypes elements stateValueHolder streamPosition expectedState expectedPosition '
	classVariableNames: 'ExpressionBinaryLiteralOperators ExpressionUnaryLiteralOperators TransitionSignal '
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionBinaryOperatorNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionFilterNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionLiteralNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionOperationNoArgsNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionOperationNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionRootNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionTraversalNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMExpressionNode subclass: #CMExpressionUnaryOperatorNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

Object subclass: #CMExpressionParser
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

CMMetaInfoPersistencyHolder subclass: #CMExpressionsMetaInfoHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

SubApplication subclass: #CODE_META_Expressions
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

TranslationsPersistencyHolder subclass: #CMExpressionsTranslationHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Expressions becomeDefault!

!CMExpressionNode class publicMethodsFor: 'instance creation'!

fromStream: theStream contextTypes: theTypes
	| aNode |
	theStream isNil ifTrue: [ ^nil].
	theStream atEnd ifTrue: [ ^nil].
	
	aNode := self new.
	aNode kind: nil initFromStream: theStream contextTypes: theTypes.
	^aNode!

kind: theKind fromStream: theStream contextTypes: theTypes
	| aNode |
	theStream isNil ifTrue: [ ^nil].
	theStream atEnd ifTrue: [ ^nil].
	
	aNode := self new.
	aNode kind: theKind initFromStream: theStream contextTypes: theTypes.
	^aNode! !

!CMExpressionNode class publicMethodsFor: 'node kinds'!

literalNumberNodeKind
	^#number! !

!CMExpressionNode class publicMethodsFor: 'operators'!

expressionBinaryLiteralOperators
	^ExpressionBinaryLiteralOperators!

expressionUnaryLiteralOperators
	^ExpressionUnaryLiteralOperators!

literalDelimiterExpressionChar
	^#'"'!

literalSetEndDelimiterExpressionChar
	^#']'!

literalSetStartDelimiterExpressionChar
	^#'['! !

!CMExpressionNode class publicMethodsFor: 'preferences'!

preferredLiteralNodeClass
	^CMExpressionLiteralNode! !

!CMExpressionNode class publicMethodsFor: 'signals'!

transitionSignal
	TransitionSignal isNil ifTrue: [
		TransitionSignal := Signal genericSignal newSignalMayProceed: true
	].
	^TransitionSignal! !

!CMExpressionNode publicMethodsFor: 'accessing'!

elements
	^elements!

elementsAdd: theElement
	elements isNil ifTrue: [ elements := OrderedCollection new: 4].
	elements add: theElement!

kind
	^kind!

kind: theKind
	kind := theKind!

state: theState
	stateValueHolder isNil ifTrue: [ stateValueHolder := nil asValue].
	stateValueHolder value: theState!

stateValueHolder
	^stateValueHolder! !

!CMExpressionNode publicMethodsFor: 'initialize-release'!

kind: theKind initFromStream: theStream contextTypes: theTypes

	theStream isNil ifTrue: [ ^nil].
	kind := theKind.
	contextTypes := theTypes.

	self parseFromStream: theStream! !

!CMExpressionNode publicMethodsFor: 'preferences'!

preferredLiteralNodeClass
	^self class preferredLiteralNodeClass! !

!CMExpressionNode publicMethodsFor: 'scanning'!

advanceToNext: theStream 

	| aChar |
	[ 	theStream atEnd not and: [ 
			aChar := theStream peek.
			aChar isSeparator 
				ifTrue: [ false]
				ifFalse: [ theStream next. true ]
		]
	] whileTrue.!

getLiteral: theStream 

	| aLiteralStream |

	aLiteralStream := WriteStream on: (String new: 32).
	[ 	| aChar |
		theStream atEnd not and: [ 
			aChar := theStream peek.
			aChar = self class literalDelimiterExpressionChar 
				ifTrue: [ theStream next . false]
				ifFalse: [ aLiteralStream add: aChar.  theStream next. true ]
		]
	] whileTrue.
	^aLiteralStream contents!

getNumber: theStream 

 	| aAlreadyDecimal aNumberStream |
	aAlreadyDecimal := false.
	aNumberStream := WriteStream on: (String new: 32).
	[ 	| aChar |
		theStream atEnd not and: [ 
			aChar := theStream peek.
			aChar isDigit 
				ifTrue: [ aNumberStream add: aChar.  theStream next. true ]
				ifFalse: [
					aChar = $. 
						ifTrue: [ 
							aAlreadyDecimal ifTrue: [ self transition: #twoDecimalDots].
							aAlreadyDecimal := true.
							aNumberStream add: aChar.  theStream next. true ]
						ifFalse: [ self transition: #lettersInNumber]
				]
		]
	] whileTrue.
	^aNumberStream contents!

getToken: theStream 

	| aTokenStream  |
	aTokenStream := WriteStream on: (String new: 32).
	[ 	| aChar |
		theStream atEnd not and: [ 
			aChar := theStream peek.
			aChar isSeparator 
				ifTrue: [ false]
				ifFalse: [ aTokenStream add: aChar.  theStream next. true ]
		]
	] whileTrue.
	^aTokenStream contents!

transition: theNewState

	self state: theNewState. 
	self class transitionSignal raise!

unexpected: theStream

	expectedState := self stateValueHolder value. 
	expectedPosition := theStream position. 

	self state: #unexpected. 
	
	self class transitionSignal raise!

unexpected: theStream position: thePosition

	expectedState := self stateValueHolder value. 
	expectedPosition := thePosition. 

	self state: #unexpected. 
	
	self class transitionSignal raise! !

!CMExpressionParser class publicMethodsFor: 'parsing'!

scanExpression: theExpression
	"Scanner new scanTokens: 'calc noclone tal cual ( sub1 sub2 ) '"

	| aStream aTransitionEx aStack anAdvanceToNextTokenBlock aState aRootExpressionNode aNewExpressionNodeBlock aCurrentExpressionNode aEndExpressionNodeBlock someStates aChar aCharSymbol aLiteralStream aLiteral aOperator aStateBlock aGetTokenBlock aTokenStream aGetLiteralBlock aEndMessageNodeBlock aNumber aGetNumberBlock aNumberStream aExpected |

	(theExpression isNil or: [ theExpression isEmpty]) ifTrue: [ ^nil].
	
	aStream := ReadStream on: theExpression.
	aTransitionEx := Signal genericSignal newSignalMayProceed: true.

	aStack := OrderedCollection new: 32.

	anAdvanceToNextTokenBlock := [
		aStream atEnd ifTrue: [ aState := #end. aTransitionEx raise].
		aStream skipSeparators.
		aStream atEnd ifTrue: [ aState := #end. aTransitionEx raise].
	].

	aGetTokenBlock := [
		aTokenStream := WriteStream on: (String new: 32).
		[ 	aStream atEnd not and: [ 
				aChar := aStream peek.
				aChar isSeparator 
					ifTrue: [ false]
					ifFalse: [ aTokenStream add: aChar.  aStream next. true ]
			]
		] whileTrue.
		aTokenStream contents
	].

	aGetLiteralBlock := [
		aLiteralStream := WriteStream on: (String new: 32).
		[ 	aStream atEnd not and: [ 
				aChar := aStream peek.
				aChar = self initExpressionOperatorLiteralDelimiter 
					ifTrue: [ aStream next . false]
					ifFalse: [ aLiteralStream add: aChar.  aStream next. true ]
			]
		] whileTrue.
		aLiteralStream contents
	].

	aGetNumberBlock := [ | aAlreadyDecimal |
		aAlreadyDecimal := false.
		aNumberStream := WriteStream on: (String new: 32).
		[ 	aStream atEnd not and: [ 
				aChar := aStream peek.
				aChar isDigit 
					ifTrue: [ aNumberStream add: aChar.  aStream next. true ]
					ifFalse: [
						aChar = $. 
							ifTrue: [ 
								aAlreadyDecimal ifTrue: [ aState := #twoDecimalDots. aTransitionEx raise].
								aAlreadyDecimal := true.
								aNumberStream add: aChar.  aStream next. true ]
							ifFalse: [ aState := #lettersInNumber. aTransitionEx raise]
					]
			]
		] whileTrue.
		aNumberStream contents
	].
	aNewExpressionNodeBlock := [
		aCurrentExpressionNode := CMExpressionNode new.
		aStack add: aCurrentExpressionNode.
		aCurrentExpressionNode
	].

	aEndExpressionNodeBlock := [
		aStack isEmpty ifTrue: [ aState := #stackUnderflow. aTransitionEx raise].
		aCurrentExpressionNode := aStack last.
		aStack removeLast.
	].

	aEndMessageNodeBlock := [
		aStack isEmpty ifTrue: [ aState := #stackUnderflow. aTransitionEx raise].
		aCurrentExpressionNode := aStack last.
		aStack removeLast.
	].

	someStates := IdentityDictionary new: 37.

	aState := #start.

		someStates at: #start put: [ 
			aNewExpressionNodeBlock value.
			aState := #scanForTraversalOrSubOrLiteral
		].
		someStates at: #scanForTraversalOrSubOrLiteral put: [ "self halt: aState."
			anAdvanceToNextTokenBlock value.
			aChar := aStream peek.
			aChar = self initExpressionOperatorLiteralDelimiter ifTrue: [  aState := #literalStart. aTransitionEx raise].
			aChar isDigit ifTrue: [  aState := #numberStart. aTransitionEx raise].
			aChar = $: ifTrue: [  aExpected := aState. aState := #unexpectedUnary. aTransitionEx raise].
			aCharSymbol :=(String with: aChar) aSymbol.
			(self expressionUnaryOperators includes: aCharSymbol) ifTrue: [  aState := #unaryOperatorStart. aTransitionEx raise].
			(self expressionBinaryOperators includes: aCharSymbol) ifTrue: [  aState := #binaryOperatorStart. aTransitionEx raise].
			aChar = self initExpressionStartSubExpression ifTrue: [  aState := #subExpressionStart. aTransitionEx raise].
			aState := #traversalStart
		].
		someStates at: #traversalStart put: [ "self halt: aState." 
			aCurrentExpressionNode kind: #traversal.
			aOperator := aGetTokenBlock value.
			aCurrentExpressionNode addElement: aOperator.
			aEndExpressionNodeBlock value.
			aState := #scanForToken
		].
		someStates at: #scanForTraversalOrFilterOrUnaryOrBinary put: [ "self halt: aState."
			anAdvanceToNextTokenBlock value.
			aChar := aStream peek.
			aChar = self initExpressionOperatorLiteralDelimiter ifTrue: [  aState := #literalStart. aTransitionEx raise].
			aChar isDigit ifTrue: [  aState := #numberStart. aTransitionEx raise].
			aChar = $: ifTrue: [  aState := #unaryOperatorStart. aTransitionEx raise].
			aCharSymbol :=(String with: aChar) aSymbol.
			(self expressionUnaryOperators includes: aCharSymbol) ifTrue: [  aState := #unaryOperatorStart. aTransitionEx raise].
			(self expressionBinaryOperators includes: aCharSymbol) ifTrue: [  aState := #binaryOperatorStart. aTransitionEx raise].
			aChar = self initExpressionStartSubExpression ifTrue: [  aState := #subExpressionStart. aTransitionEx raise].
			aChar = self initExpressionStartFilterExpression ifTrue: [  aState := #filterExpressionStart. aTransitionEx raise].
			aState := #traversalStart
		].
		someStates at: #literalStart put: [ "self halt: aState." 
			aCurrentExpressionNode kind: #literal.
			aLiteral := aGetLiteralBlock value.
			aCurrentExpressionNode addElement: aLiteral.
			aState := #scanForToken
		].
		someStates at: #numberStart put: [ "self halt: aState." 
			aCurrentExpressionNode kind: #number.
			aNumber := aGetNumberBlock value.
			aCurrentExpressionNode addElement: aNumber.
			aState := #scanForToken
		].
		someStates at: #unaryOperatorStart put: [ "self halt: aState." 
			aCurrentExpressionNode kind: #unary.
			aOperator := aGetTokenBlock value.
			aCurrentExpressionNode addElement: aOperator.
			aEndExpressionNodeBlock value.
			aState := #scanForToken
		].
		someStates at: #binaryOperatorStart put: [ "self halt: aState." 
			aCurrentExpressionNode kind: #binary.
			aOperator := aGetTokenBlock value.
			aCurrentExpressionNode addElement: aOperator.
			aState := #scanForSecondToken
		].

		someStates at: #scanForSecondToken put: [ "self halt: aState."
			anAdvanceToNextTokenBlock value.
			aChar := aStream peek.
			aChar = self initExpressionOperatorLiteralDelimiter ifTrue: [  aState := #literalStart. aTransitionEx raise].
			aChar isDigit ifTrue: [  aState := #numberStart. aTransitionEx raise].
			aChar = $: ifTrue: [  aState := #unaryOperatorStart. aTransitionEx raise].
			aCharSymbol :=(String with: aChar) aSymbol.
			(self expressionUnaryOperators includes: aCharSymbol) ifTrue: [  aState := #unaryOperatorStart. aTransitionEx raise].
			(self expressionBinaryOperators includes: aCharSymbol) ifTrue: [  aState := #binaryOperatorStart. aTransitionEx raise].
			aChar = self initExpressionStartSubExpression ifTrue: [  aState := #subExpressionStart. aTransitionEx raise].
			aState := #featureNameStart
		].
		


	aRootExpressionNode := aNewExpressionNodeBlock value.

	[ aState = #end] whileFalse: [ 
		aStateBlock := someStates at: aState ifAbsent: [nil].
		aStateBlock isNil ifTrue: [ self halt: 'State ', aState, ' not found in automata'. ^nil].

		self trace ifTrue:  [ Transcript show: aState; cr.].
		
		aTransitionEx handle: [:anEx | ] do: [ aStateBlock value].
	].! !

!CMExpressionRootNode publicMethodsFor: 'initialize-release'!

initFromStream: theStream contextTypes: theTypes

	| someStates aChar aCharSymbol aStateBlock aNode someContextTypes aPosition someAfterTraversalContextTypes |
	theStream isNil ifTrue: [ ^nil].

	OrderedCollection new: 32.

	someContextTypes := theTypes.
	
	someStates := IdentityDictionary new: 37.

	someStates at: #start put: [ 
		self transition: #scanForTraversalOrSubOrLiteral
	].
	someStates at: #scanForTraversalOrSubOrLiteral put: [ "self halt: aState."
		self advanceToNext: theStream.
		aChar := theStream peek.
		aChar = self class literalDelimiterExpressionChar ifTrue: [  self transition: #literalStart].
		aChar = self literalSetStartDelimiterExpressionChar ifTrue: [  self transition: #literalSetStart].
		aChar isDigit ifTrue: [   self transition: #numberStart].
		aChar = self initExpressionStartSubExpression ifTrue: [ self transition: #subExpressionStart].
		self transition: #traversalStart
	].
	someStates at: #literalStart put: [ "self halt: aState." 
		aNode := self preferredLiteralNodeClass fromStream: theStream contextTypes: someContextTypes.
		aNode isNil ifFalse: [ self elementsAdd: aNode].
		self transition:  #scanForLiteralUnaryOrLiteralBinary
	].
	someStates at: #scanForLiteralUnaryOrLiteralBinary put: [ "self halt: aState."
		self advanceToNext: theStream.
		aChar := theStream peek.
		aChar = self initExpressionOperatorLiteralDelimiter ifTrue: [ self unexpected: theStream].
		aChar isDigit ifTrue: [ self unexpected: theStream].
		aChar = self initExpressionStartSubExpression ifTrue: [ self unexpected: theStream].
		aChar = $: ifTrue: [ self transition: #unaryOperatorStart].
		aCharSymbol :=(String with: aChar) aSymbol.
		(self class expressionUnaryLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #unaryOperatorStart].
		(self class expressionBinaryLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #binaryOperatorStart].
		self  unexpected: theStream
	].
	someStates at: #numberStart put: [ "self halt: aState." 
		aNode := self preferredLiteralNodeClass kind: self class literalNumberNodeKind fromStream: theStream contextTypes: someContextTypes.
		aNode isNil ifFalse: [ self elementsAdd: aNode].
		self transition: #scanForLiteralUnaryOrLiteralBinary
	].
	someStates at: #literalSetStart put: [ "self halt: aState." 
		aNode := self preferredLiteralNodeClass kind: self class literalSetNodeKind fromStream: theStream contextTypes: someContextTypes.
		aNode isNil ifFalse: [ self elementsAdd: aNode].
		self transition:  #scanForLiteralSetUnaryOrLiteralBinary
	].
	someStates at: #scanForLiteralSetUnaryOrLiteralBinary put: [ "self halt: aState."
		self advanceToNext: theStream.
		aChar := theStream peek.
		aChar = self literalDelimiterExpressionChar ifTrue: [ self unexpected: theStream].
		aChar isDigit ifTrue: [ self unexpected: theStream].
		aChar = self subExpressionStartExpressionChar ifTrue: [ self unexpected: theStream].
		aChar = $: ifTrue: [ self transition: #unarySetOperatorStart].
		aCharSymbol :=(String with: aChar) aSymbol.
		(self class expressionUnarySetLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #unarySetOperatorStart].
		(self class expressionBinarySetLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #binarySetOperatorStart].
		self  unexpected: theStream
	].
	someStates at: #traversalStart put: [ "self halt: aState." 
		aPosition := theStream position.
		aNode := self preferredTraversalNodeClass kind: self class traversalNodeKind fromStream: theStream contextTypes: someContextTypes.
		aNode isNil ifTrue: [ self unexpected: theStream position: aPosition].
		self elementsAdd: aNode.
		someAfterTraversalContextTypes := aNode afterTraversalContextTypes.
		someAfterTraversalContextTypes isNil ifFalse: [ someContextTypes := someAfterTraversalContextTypes].
 		self transition: #scanForTraversalOrFilterOrUnaryOrBinary
	].


	self state: #start.

	[ self stateValueHolder value == #end] whileFalse: [ 
		aStateBlock := someStates at: self stateValueHolder value ifAbsent: [nil].
		aStateBlock isNil ifTrue: [ self halt: 'State ', self stateValueHolder value, ' not found in automata'. ^nil].

		self trace ifTrue:  [ Transcript show: self stateValueHolder value; cr.].
		
		self class transitionSignal handle: [:anEx | ] do: [ aStateBlock value].
	].!

parseFromStream: theStream 

	| someStates aChar aCharSymbol aStateBlock aNode someContextTypes aPosition someAfterTraversalContextTypes theTypes |
	theStream isNil ifTrue: [ ^nil].

	OrderedCollection new: 32.

	someContextTypes := theTypes.
	
	someStates := IdentityDictionary new: 37.

	someStates at: #start put: [ 
		self transition: #scanForTraversalOrSubOrLiteral
	].
	someStates at: #scanForTraversalOrSubOrLiteral put: [ "self halt: aState."
		self advanceToNext: theStream.
		aChar := theStream peek.
		aChar = self class literalDelimiterExpressionChar ifTrue: [  self transition: #literalStart].
		aChar = self literalSetStartDelimiterExpressionChar ifTrue: [  self transition: #literalSetStart].
		aChar isDigit ifTrue: [   self transition: #numberStart].
		aChar = self initExpressionStartSubExpression ifTrue: [ self transition: #subExpressionStart].
		self transition: #traversalStart
	].
	someStates at: #literalStart put: [ "self halt: aState." 
		aNode := self preferredLiteralNodeClass fromStream: theStream contextTypes: self contextTypes.
		aNode isNil ifFalse: [ self elementsAdd: aNode].
		self transition:  #scanForLiteralUnaryOrLiteralBinary
	].
	someStates at: #scanForLiteralUnaryOrLiteralBinary put: [ "self halt: aState."
		self advanceToNext: theStream.
		aChar := theStream peek.
		aChar = self initExpressionOperatorLiteralDelimiter ifTrue: [ self unexpected: theStream].
		aChar isDigit ifTrue: [ self unexpected: theStream].
		aChar = self initExpressionStartSubExpression ifTrue: [ self unexpected: theStream].
		aChar = $: ifTrue: [ self transition: #unaryOperatorStart].
		aCharSymbol :=(String with: aChar) aSymbol.
		(self class expressionUnaryLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #unaryOperatorStart].
		(self class expressionBinaryLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #binaryOperatorStart].
		self  unexpected: theStream
	].
	someStates at: #numberStart put: [ "self halt: aState." 
		aNode := self preferredLiteralNodeClass kind: self class literalNumberNodeKind fromStream: theStream contextTypes: self contextTypes.
		aNode isNil ifFalse: [ self elementsAdd: aNode].
		self transition: #scanForLiteralUnaryOrLiteralBinary
	].
	someStates at: #literalSetStart put: [ "self halt: aState." 
		aNode := self preferredLiteralNodeClass kind: self class literalSetNodeKind fromStream: theStream contextTypes: self contextTypes.
		aNode isNil ifFalse: [ self elementsAdd: aNode].
		self transition:  #scanForLiteralSetUnaryOrLiteralBinary
	].
	someStates at: #scanForLiteralSetUnaryOrLiteralBinary put: [ "self halt: aState."
		self advanceToNext: theStream.
		aChar := theStream peek.
		aChar = self literalDelimiterExpressionChar ifTrue: [ self unexpected: theStream].
		aChar isDigit ifTrue: [ self unexpected: theStream].
		aChar = self subExpressionStartExpressionChar ifTrue: [ self unexpected: theStream].
		aChar = $: ifTrue: [ self transition: #unarySetOperatorStart].
		aCharSymbol :=(String with: aChar) aSymbol.
		(self class expressionUnarySetLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #unarySetOperatorStart].
		(self class expressionBinarySetLiteralOperators includes: aCharSymbol) ifTrue: [ self transition: #binarySetOperatorStart].
		self  unexpected: theStream
	].
	someStates at: #traversalStart put: [ "self halt: aState." 
		aPosition := theStream position.
		aNode := self preferredTraversalNodeClass kind: self class traversalNodeKind fromStream: theStream contextTypes: self contextTypes.
		aNode isNil ifTrue: [ self unexpected: theStream position: aPosition].
		self elementsAdd: aNode.
		someAfterTraversalContextTypes := aNode afterTraversalContextTypes.
		someAfterTraversalContextTypes isNil ifFalse: [ someAfterTraversalContextTypes].
 		self transition: #scanForTraversalOrFilterOrUnaryOrBinary
	].


	self state: #start.

	[ self stateValueHolder value == #end] whileFalse: [ 
		aStateBlock := someStates at: self stateValueHolder value ifAbsent: [nil].
		aStateBlock isNil ifTrue: [ self halt: 'State ', self stateValueHolder value, ' not found in automata'. ^nil].

		self trace ifTrue:  [ Transcript show: self stateValueHolder value; cr.].
		
		self class transitionSignal handle: [:anEx | ] do: [ aStateBlock value].
	].! !

!CMExpressionsMetaInfoHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentModel browsePath"
	"self  resetCurrentModels"
	"(self  currentModelStoreMethodSelector: self defaultCurrentModelSelector) browsePath"
	"self  resetCurrentModelStoreMethodSelector: self defaultCurrentModelSelector"!

defaultCurrentModelSelector
	"self  defaultCurrentTranslationSelector "

	^#expressionsModelStore!

retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector
	| aModel |
	aModel := super retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector.
	aModel isNil ifTrue: [ ^nil].

	aModel definitionsHolderClassNameForInstances: CODEMModelDefinitionsHolder name.
	aModel configurationClassNameForInstances: CODEMModelMETAConfiguration name.
	^aModel! !

!CMExpressionsMetaInfoHolder class publicMethodsFor: 'modelElements persistence'!

expressionsModelStore

	"(CODEElement newFromPersistenceAsCode: CMExpressionsMetaInfoHolder expressionsModelStore) browsePath"

	self ojoModel.

	^   #( model 'Expressions'
	nil nil
	nil
	nil
	CMExpressionsMetaInfoHolder expressionsModelStore
	nil
	(submodules
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
			nil nil
			'CODEModel'
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
			(supertypes
			   ( refToType 'Object' 'PrimitiveTypes'  ) 
			 )
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
			nil nil
			'CODEElement'
			false false false false
			nil
			nil
			nil
			(relationships
			  ( relationship 'resultTypedNode'
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
				( refToInverseRelationship 'resultTypes'  ( refToType 'ResultTypedNode' 'Expressions'  )  ) 
			   )

			 )
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
			(supertypes
			   ( refToType 'Object' 'PrimitiveTypes'  ) 
			 )
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
			(supertypes
			   ( refToType 'Object' 'PrimitiveTypes'  ) 
			 )
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
			(supertypes
			   ( refToType 'Object' 'PrimitiveTypes'  ) 
			 )
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
			(supertypes
			   ( refToType 'Object' 'PrimitiveTypes'  ) 
			 )
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
			(supertypes
			   ( refToType 'Object' 'PrimitiveTypes'  ) 
			 )
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
			(supertypes
			   ( refToType 'Object' 'PrimitiveTypes'  ) 
			 )
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
		nil
	   )

	  ( module 'Expressions'
		nil nil
		nil
		nil
		nil nil
		(types
		  ( type 'Node'
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

		  ( type 'ResultTypedNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'Node' 'Expressions'  ) 
			 )
			nil
			(relationships
			  ( relationship 'resultTypes'
				nil nil
				nil
				nil
				REFERENCES nil
				#'0' #*
				false false true false false false false
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'resultTypedNode'  ( refToType 'CODEType' 'PrimitiveTypes'  )  ) 
			   )

			  ( relationship 'multiNode'
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
				( refToInverseRelationship 'nodes'  ( refToType 'MultiNode' 'Expressions'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'LiteralNode'
			nil nil
			nil
			nil
			true false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ResultTypedNode' 'Expressions'  ) 
			 )
			(attributes
			  ( attribute 'literal'
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
				 ( refToType 'Object' 'PrimitiveTypes'  ) 

			   )

			 )
			(relationships
			  ( relationship 'literalCollectionNode'
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
				( refToInverseRelationship 'literalNodes'  ( refToType 'LiteralCollectionNode' 'Expressions'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'NumberNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LiteralNode' 'Expressions'  ) 
			 )
			(attributes
			  ( attributeRefinement 'literal'
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

				(refinedAttributes
				   ( refToAttribute 'literal'  ( refToType 'LiteralNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
			nil
		   )

		  ( type 'StringNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LiteralNode' 'Expressions'  ) 
			 )
			(attributes
			  ( attributeRefinement 'literal'
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

				(refinedAttributes
				   ( refToAttribute 'literal'  ( refToType 'LiteralNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
			nil
		   )

		  ( type 'DateNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LiteralNode' 'Expressions'  ) 
			 )
			(attributes
			  ( attributeRefinement 'literal'
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
				 ( refToType 'Date' 'PrimitiveTypes'  ) 

				(refinedAttributes
				   ( refToAttribute 'literal'  ( refToType 'LiteralNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
			nil
		   )

		  ( type 'TimeNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LiteralNode' 'Expressions'  ) 
			 )
			(attributes
			  ( attributeRefinement 'literal'
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
				 ( refToType 'Time' 'PrimitiveTypes'  ) 

				(refinedAttributes
				   ( refToAttribute 'literal'  ( refToType 'LiteralNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
			nil
		   )

		  ( type 'TextNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LiteralNode' 'Expressions'  ) 
			 )
			(attributes
			  ( attributeRefinement 'literal'
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
				 ( refToType 'Text' 'PrimitiveTypes'  ) 

				(refinedAttributes
				   ( refToAttribute 'literal'  ( refToType 'LiteralNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
			nil
		   )

		  ( type 'IntegerNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'LiteralNode' 'Expressions'  ) 
			 )
			(attributes
			  ( attributeRefinement 'literal'
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

				(refinedAttributes
				   ( refToAttribute 'literal'  ( refToType 'LiteralNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
			nil
		   )

		  ( type 'LiteralCollectionNode'
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
			  ( relationship 'literalNodes'
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
				( refToInverseRelationship 'literalCollectionNode'  ( refToType 'LiteralNode' 'Expressions'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'ComputationNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ResultTypedNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'MultiNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ComputationNode' 'Expressions'  ) 
			 )
			nil
			(relationships
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
				( refToInverseRelationship 'multiNode'  ( refToType 'ResultTypedNode' 'Expressions'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'BinaryNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ComputationNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'SelfNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ResultTypedNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'SequenceNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'MultiNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'CascadeNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'MultiNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'AlternatesNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'MultiNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'GuardedAlternatesNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'MultiNode' 'Expressions'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'nodes'
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
				( refToInverseRelationship 'multiNode'  ( refToType 'GuardedNode' 'Expressions'  )  ) 
				(refinedRelationships
				  ( refToRefinedRelationship 'nodes'  ( refToType 'MultiNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
		   )

		  ( type 'GuardedNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ResultTypedNode' 'Expressions'  ) 
			 )
			nil
			(relationships
			  ( relationshipRefinement 'multiNode'
				nil nil
				nil
				nil
				isAGGREGATED nil
				#'1' #'1'
				false false false false false true true
				NOCOMPUTATION
				''
				''
				''
				''
				( refToInverseRelationship 'nodes'  ( refToType 'GuardedAlternatesNode' 'Expressions'  )  ) 
				(refinedRelationships
				  ( refToRefinedRelationship 'multiNode'  ( refToType 'ResultTypedNode' 'Expressions'  )  ) 
				 )
			   )

			 )
			nil
		   )

		  ( type 'TypeGuardedNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'GuardedNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'ExpressionGuardedNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'GuardedNode' 'Expressions'  ) 
			 )
			nil
			(relationships
			  ( relationship 'guardExpressionRootNodes'
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
				( refToInverseRelationship 'expressionGuardedNode'  ( refToType 'GuardExpressionRootNode' 'Expressions'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'ExpressionRootNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ResultTypedNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		  ( type 'GuardExpressionRootNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ExpressionRootNode' 'Expressions'  ) 
			 )
			nil
			(relationships
			  ( relationship 'expressionGuardedNode'
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
				( refToInverseRelationship 'guardExpressionRootNodes'  ( refToType 'ExpressionGuardedNode' 'Expressions'  )  ) 
			   )

			 )
			nil
		   )

		  ( type 'UnaryNode'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'ComputationNode' 'Expressions'  ) 
			 )
			nil
			nil
			nil
		   )

		 )
		nil
	   )

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
			nil
			nil
			nil
		   )

		 )
		nil
	   )

	 )
	CMExpressionsTranslationHolder expressionsTranslationStore
   )! !

!CMExpressionsTranslationHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentTranslation browsePath"
	"self  resetCurrentTranslations"
	"(self  currentTranslationStoreMethodSelector: self defaultCurrentTranslationSelector) browsePath"
	"self  resetTranslationSelectorsToIgnore"
	"self  translationSelectorsToIgnore"
	"self  translationSelectorsToIgnoreAdd: self defaultCurrentTranslationSelector"! !

!CMExpressionsTranslationHolder class publicMethodsFor: 'default'!

defaultCurrentTranslationSelector
	"CMExpressionsTranslationHolder  defaultCurrentTranslationSelector "

	^#expressionsTranslationStore! !

!CMExpressionsTranslationHolder class publicMethodsFor: 'translations persistence'!

expressionsTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMExpressionsTranslationHolder expressionsTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'Expressions_ApplicationTranslation'
	CMExpressionsTranslationHolder expressionsTranslationStore
	nil
	nil
   )! !

CMExpressionNode initializeAfterLoad!
CMExpressionBinaryOperatorNode initializeAfterLoad!
CMExpressionFilterNode initializeAfterLoad!
CMExpressionLiteralNode initializeAfterLoad!
CMExpressionOperationNoArgsNode initializeAfterLoad!
CMExpressionOperationNode initializeAfterLoad!
CMExpressionRootNode initializeAfterLoad!
CMExpressionTraversalNode initializeAfterLoad!
CMExpressionUnaryOperatorNode initializeAfterLoad!
CMExpressionParser initializeAfterLoad!
CMExpressionsMetaInfoHolder initializeAfterLoad!
CODE_META_Expressions initializeAfterLoad!
CMExpressionsTranslationHolder initializeAfterLoad!

CODE_META_Expressions loaded!
