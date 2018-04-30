'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META_DefinedUI createSubApplication: #CODE_META_DefinedUI_Specs in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_DefinedUI_Specs becomeDefault!

InputBoxController subclass: #CMRDOnlyInputBoxController
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

SubApplication subclass: #CODE_META_DefinedUI_Specs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

LabelSpec subclass: #CMLabelSpec
	instanceVariableNames: 'definedPart definedEditor visibilityAdaptor initiallyVisible enablementAdaptor initiallyEnabled '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

ActionButtonSpec subclass: #CMActionButtonSpec
	instanceVariableNames: 'definedPart definedEditor visibilityAdaptor initiallyVisible enablementAdaptor initiallyEnabled '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

RadioButtonSpec subclass: #CMRadioButtonSpec
	instanceVariableNames: 'definedPart definedEditor visibilityAdaptor initiallyVisible enablementAdaptor initiallyEnabled '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

InputFieldSpec subclass: #CMInputFieldSpec
	instanceVariableNames: 'printConverter definedPart definedEditor visibilityAdaptor initiallyVisible enablementAdaptor initiallyEnabled '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

BorderedWrapper subclass: #CMBorderedWrapper
	instanceVariableNames: 'cooperativeRectangle '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

BoundedWrapper subclass: #CMBoundedWrapper
	instanceVariableNames: 'cooperativeRectangle '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_DefinedUI_Specs becomeDefault!

!CMActionButtonSpec publicMethodsFor: 'accessing'!

definedEditor
	^definedEditor!

definedEditor: theDefinedEditor
	definedEditor := theDefinedEditor!

definedPart
	^definedPart

"*VIPVersion 9-7-97 | 6:05:16 pm 'ACV'*"!

definedPart: theDefinedPart
	definedPart := theDefinedPart!

enablementAdaptor
	^enablementAdaptor!

enablementAdaptor: theAdaptor
	enablementAdaptor := theAdaptor!

initiallyEnabled
	^initiallyEnabled!

initiallyEnabled: theBool
	initiallyEnabled := theBool == true!

initiallyVisible
	^initiallyVisible!

initiallyVisible: theBool
	initiallyVisible := theBool == true!

visibilityAdaptor
	^visibilityAdaptor!

visibilityAdaptor: theAdaptor
	visibilityAdaptor := theAdaptor! !

!CMActionButtonSpec publicMethodsFor: 'private'!

dispatchTo: policy with: builder

	policy cmActionButton: self into: builder! !

!CMBorderedWrapper publicMethodsFor: 'accessing'!

computeDisplayBoxFor: aComponent inDisplayBox: aRectangle
	"Answer a rectangle for aComponent to use as its display box."

	^cooperativeRectangle isNil 
		ifFalse: [ cooperativeRectangle rectangleRelativeTo: aRectangle] 
		ifTrue: [ super computeDisplayBoxFor: aComponent inDisplayBox: aRectangle]!

cooperativeRectangle
	^cooperativeRectangle!

cooperativeRectangle: theRect
	cooperativeRectangle := theRect!

setCooperativeRectangle: theRect
	cooperativeRectangle := theRect! !

!CMBorderedWrapper publicMethodsFor: 'bounds accessing'!

bounds
	"Answer the receiver's actual bounds."

	^super bounds!

bounds: newBounds
	"Answer the receiver's actual bounds."

	^super bounds: newBounds!

rectangleRelativeTo: containersBoundingBox
	"Forward to the receiver's layout for computing a bounding box.
	This can be any object that responds to #rectangleRelativeTo: preferred:.
	See class Layout."

	^cooperativeRectangle isNil 
		ifFalse: [ cooperativeRectangle rectangleRelativeTo: containersBoundingBox] 
		ifTrue: [ super rectangleRelativeTo: containersBoundingBox]! !

!CMBoundedWrapper publicMethodsFor: 'accessing'!

computeDisplayBoxFor: aComponent inDisplayBox: aRectangle
	"Answer a rectangle for aComponent to use as its display box."

	^cooperativeRectangle isNil 
		ifFalse: [ cooperativeRectangle rectangleRelativeTo: aRectangle]
		ifTrue: [ super computeDisplayBoxFor: aComponent inDisplayBox: aRectangle]!

cooperativeRectangle
	^cooperativeRectangle!

cooperativeRectangle: theRect
	cooperativeRectangle := theRect!

setCooperativeRectangle: theRect
	cooperativeRectangle := theRect! !

!CMBoundedWrapper publicMethodsFor: 'bounds accessing'!

bounds
	"Answer the receiver's actual bounds."

	^super bounds!

bounds: newBounds
	"Answer the receiver's actual bounds."

	^super bounds: newBounds!

rectangleRelativeTo: containersBoundingBox
	"Forward to the receiver's layout for computing a bounding box.
	This can be any object that responds to #rectangleRelativeTo: preferred:.
	See class Layout."

	^cooperativeRectangle isNil 
		ifFalse: [  cooperativeRectangle rectangleRelativeTo: containersBoundingBox]
		ifTrue: [ super rectangleRelativeTo: containersBoundingBox]! !

!CMInputFieldSpec class publicMethodsFor: 'preferences'!

preferredPrintConverterClass

	^CMPrintConverter! !

!CMInputFieldSpec publicMethodsFor: 'accessing'!

converterWith: aBuilder
	^self printConverter!

definedEditor
	^definedEditor!

definedEditor: theDefinedEditor
	definedEditor := theDefinedEditor!

definedPart
	^definedPart

"*VIPVersion 9-7-97 | 6:05:16 pm 'ACV'*"!

definedPart: theDefinedPart
	definedPart := theDefinedPart!

enablementAdaptor
	^enablementAdaptor!

enablementAdaptor: theAdaptor
	enablementAdaptor := theAdaptor!

initiallyEnabled
	^initiallyEnabled!

initiallyEnabled: theBool
	initiallyEnabled := theBool == true!

initiallyVisible
	^initiallyVisible!

initiallyVisible: theBool
	initiallyVisible := theBool == true!

printConverter
	^printConverter!

printConverter: thePrintConverter
	printConverter := thePrintConverter!

visibilityAdaptor
	^visibilityAdaptor!

visibilityAdaptor: theAdaptor
	visibilityAdaptor := theAdaptor! !

!CMInputFieldSpec publicMethodsFor: 'private'!

dispatchTo: policy with: builder

	policy cmInputBox: self into: builder! !

!CMLabelSpec publicMethodsFor: 'accessing'!

definedEditor
	^definedEditor!

definedEditor: theDefinedEditor
	definedEditor := theDefinedEditor!

definedPart
	^definedPart

"*VIPVersion 9-7-97 | 6:05:16 pm 'ACV'*"!

definedPart: theDefinedPart
	definedPart := theDefinedPart!

enablementAdaptor
	^enablementAdaptor!

enablementAdaptor: theAdaptor
	enablementAdaptor := theAdaptor!

initiallyEnabled
	^initiallyEnabled!

initiallyEnabled: theBool
	initiallyEnabled := theBool == true!

initiallyVisible
	^initiallyVisible!

initiallyVisible: theBool
	initiallyVisible := theBool == true!

visibilityAdaptor
	^visibilityAdaptor!

visibilityAdaptor: theAdaptor
	visibilityAdaptor := theAdaptor! !

!CMLabelSpec publicMethodsFor: 'private'!

dispatchTo: policy with: builder

	policy cmLabel: self into: builder! !

!CMRadioButtonSpec publicMethodsFor: 'accessing'!

definedEditor
	^definedEditor

"*VIPVersion 9-7-97 | 4:14:47 pm 'ACV'*"!

definedEditor: theDefinedEditor
	definedEditor := theDefinedEditor

"*VIPVersion 9-7-97 | 4:14:50 pm 'ACV'*"!

definedPart
	^definedPart

"*VIPVersion 9-7-97 | 4:09:56 pm 'ACV'*"!

definedPart: theDefinedPart
	definedPart := theDefinedPart

"*VIPVersion 9-7-97 | 4:09:46 pm 'ACV'*"!

enablementAdaptor
	^enablementAdaptor!

enablementAdaptor: theAdaptor
	enablementAdaptor := theAdaptor!

initiallyEnabled
	^initiallyEnabled!

initiallyEnabled: theBool
	initiallyEnabled := theBool == true!

initiallyVisible
	^initiallyVisible!

initiallyVisible: theBool
	initiallyVisible := theBool == true!

visibilityAdaptor
	^visibilityAdaptor!

visibilityAdaptor: theAdaptor
	visibilityAdaptor := theAdaptor! !

!CMRadioButtonSpec publicMethodsFor: 'private'!

dispatchTo: policy with: builder

	policy cmRadioButton: self into: builder! !

!CMRDOnlyInputBoxController publicMethodsFor: 'control defaults'!

controlActivity

	self processMouseButtons!

isControlWanted

	^self viewHasCursor and: [self sensor redButtonPressed or: [self sensor yellowButtonPressed]]! !

!UIBuilder publicMethodsFor: 'private'!

cmApplyLayout: theLayout

	theLayout class == Point ifTrue: [ ^self wrapper setOrigin: theLayout].

	theLayout class == Rectangle ifTrue: [ ^self wrapper setOrigin: theLayout origin extent: theLayout extent].

	theLayout class == CooperativeRectangle ifTrue: [ ^self wrapper setCooperativeRectangle: theLayout].

	self wrapper layout: theLayout! !

!UILookPolicy publicMethodsFor: 'cm-building'!

cmLabel: theSpec into: theBuilder

	|   aLabelComponent aLabelVisual | 

	(theSpec isKindOf: CMLabelSpec) ifFalse: [ ^self label: theSpec into: theBuilder].

	aLabelComponent := PassiveLabel new.

	self setStyleOf: aLabelComponent to: theSpec style.

	aLabelVisual := theSpec labelInBuilder: theBuilder.
	(theSpec hasCharacterOrientedLabel)
		ifTrue: [ aLabelComponent setLabelString: aLabelVisual]
		ifFalse: [ aLabelComponent setLabel: aLabelVisual].

	aLabelComponent widgetState isEnabled: theSpec initiallyEnabled.

	theBuilder isEditing ifFalse: [ aLabelComponent widgetState isVisible: theSpec initiallyVisible].

	theBuilder component: aLabelComponent.

	theBuilder wrapWith: (self cmSimpleWrapperFor: theSpec).

	theBuilder cmApplyLayout: theSpec layout.

	theSpec definedPart buildEnablementUpdater: theBuilder spec: theSpec.
	theSpec definedPart buildVisibilityUpdater: theBuilder  spec: theSpec.

	theBuilder wrapWith: (self simpleWidgetWrapperOn: theBuilder spec: theSpec)! !

!UILookPolicy publicMethodsFor: 'private'!

cmAdaptorForSelector: elSelector value: elValue model: elModel
	
	| anAdaptor |
	elModel isNil ifTrue: [ ^nil].

	anAdaptor := elModel adaptorForSelector: elSelector value: elValue.
	^anAdaptor!

cmSimpleWrapperFor: spec

	| aLayoutType |
	aLayoutType := self proLayoutTypeFor: spec layout.
	^aLayoutType == #translating
		ifTrue: [self translatingWrapperClass new]
		ifFalse: [
			aLayoutType == #cooperative
				ifTrue: [ CMBoundedWrapper new]
				ifFalse: [ self boundedWrapperClass new]]! !

CMRDOnlyInputBoxController initializeAfterLoad!
CODE_META_DefinedUI_Specs initializeAfterLoad!
CMLabelSpec initializeAfterLoad!
CMActionButtonSpec initializeAfterLoad!
CMRadioButtonSpec initializeAfterLoad!
CMInputFieldSpec initializeAfterLoad!
CMBorderedWrapper initializeAfterLoad!
CMBoundedWrapper initializeAfterLoad!

CODE_META_DefinedUI_Specs loaded!
