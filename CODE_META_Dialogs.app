'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_Dialogs in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_Dialogs becomeDefault!

SubApplication subclass: #CODE_META_Dialogs
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_Dialogs becomeDefault!

!SimpleDialog publicMethodsFor: 'interface construction'!

addLabels: labels images: images default: defaultValue storeInto: result takeKeyboard: takeKeyboard equalize: eqBoolean 

	^self
		addLabels: labels
		images: images
		default: defaultValue
		storeInto: result
		takeKeyboard: takeKeyboard
		equalize: eqBoolean
		columns: 3!

addLabels: labels images: images default: defaultValue storeInto: result takeKeyboard: takeKeyboard equalize: eqBoolean columns: nColumns

	| num maxButtonWidth maxButtonHeight separation buttonWAs buttonWidth window box layout left top actualColumns actualRows |
	num := labels size.
	maxButtonWidth := 0.
	maxButtonHeight := 0.
	separation := 20.
	buttonWAs := OrderedCollection new.
	window := builder window.
	box := window displayBox.
	actualColumns := nColumns min: labels size.
	actualRows := num + actualColumns - 1 // actualColumns.

	"We have to create and place the buttons before we can ask them their
	preferred bounds and refine their spacing."

	layout := LayoutFrame new.
	layout leftFraction: 0.5; rightFraction: 0.5.
	builder newComposite.
	1 to: num do:
		[:index |
		| lbl val buttonSpec buttonW bExt aLabelKey |
		lbl := images at: index.
		val := labels at: index.
		aLabelKey := (labels at: index) asSymbol.
		builder visualAt: aLabelKey put: lbl.
		(buttonSpec :=
			ActionButtonSpec
				model: (result == nil
						ifTrue: [val]
						ifFalse: [[result value: val. self close]])
				label: aLabelKey
				layout: (0@0 extent: 1@1))
			defaultable: (images includes: defaultValue);
			isDefault: val == defaultValue;
			hasImageOrientedLabel: true.
		
		builder add: buttonSpec.
		buttonW := builder wrapper.
		(takeKeyboard and: [val == defaultValue])
			ifTrue: [builder keyboardProcessor setActive: buttonW widget controller].
		maxButtonWidth := maxButtonWidth max: (bExt := buttonW preferredBounds extent) x.
		maxButtonHeight := maxButtonHeight max: bExt y.
		buttonWAs add: buttonW -> bExt].
 	buttonWidth := eqBoolean
		ifTrue: [actualColumns * maxButtonWidth + ((actualColumns - 1) * separation)]
		ifFalse: [buttonWAs
				inject: separation negated
				into: [:x :assoc | x + assoc value x + separation]].
	layout topOffset: box height; bottomOffset: box height+(actualRows * maxButtonHeight).
	layout
		leftOffset: 0 - ((buttonWidth + 1) // 2);
		rightOffset: (buttonWidth + 1) // 2.
	builder endCompositeLayout: layout.
	left := 0.
	top := 0.
	1 to: num do:
		[:index |
		| bttnWA width |
		bttnWA := buttonWAs at: index.
		width := eqBoolean
				ifTrue: [maxButtonWidth]
				ifFalse: [bttnWA value x].
		bttnWA key newLayout:
			(Rectangle
				left: left
				right: left + width
				top: top
				bottom: top + maxButtonHeight).
		index \\ actualColumns = 0
			ifTrue:
				[left := 0.
				top := top + maxButtonHeight]
			ifFalse: [left := left + width + separation]].
	self addGap: maxButtonHeight * actualRows.
	buttonWidth := buttonWidth + separation.
	self minWidth: buttonWidth.
	^builder wrapper! !

!SimpleDialog publicMethodsFor: 'utility'!

choose: messageString labels: labels images: images default: defaultValue
	"Ask the user a question.  Let the user pick from a row of buttons made up
	to match the labels collection.  Return the response from the corresponding
	item from the images collection."

	"SimpleDialog new
		choose: 'Are you tired yet?'
		labels: (Array with: 'absolutely' with: 'sort of' with: 'not really')
		images: #(#yes #maybe #no)
		default: #maybe.
	SimpleDialog new
		choose: 'Are you completely sure?'
		labels: (Array with: 'definitely' with: 'sort of no' with: 'sort of yes' with: 'nope')
		images: #(#yes #maybeNo #maybeYes #no)
		default: #no"

	^self
		choose: messageString
		labels: labels
		images: images
		default: defaultValue
		equalize: true
		for: nil!

choose: messageString labels: labels images: images default: defaultValue equalize: eqBoolean for: aVisualOrNil
	"Ask the user a question.  Let the user pick from a row of buttons made up
	to match the labels collection.  Return the response from the corresponding
	item from the images collection."

	"aVisualOrNil, if not nil, may be either a VisualPart or a
	ScheduledWindow.  It controls the look and feel and color choices
	used by the dialog, and supplies the dialog's master window, which
	is used by some window systems to create a visual connection between
	the dialog and the window that created it."

	"SimpleDialog new
		choose: 'Are you tired yet?'
		labels: (Array with: 'absolutely' with: 'sort of' with: 'not really')
		images: #(#yes #maybe #no)
		default: #maybe
		equalize: true
		for: Dialog defaultParentWindow.
	SimpleDialog new
		choose: 'Are you completely sure?'
		labels: (Array with: 'definitely' with: 'sort of no' with: 'sort of yes' with: 'nope')
		images: #(#yes #maybeNo #maybeYes #no)
		default: #no
		equalize: false
		for: Dialog defaultParentWindow"

	| result spec |
	self escapeIsCancel: false.
	result := ValueHolder new.
	spec := (self class interfaceSpecFor: #emptySpec).
	self initializeBuilderFor: aVisualOrNil.
	builder add: spec window.
	builder add: spec component.
	self initializeWindowFor: aVisualOrNil.

	self setInitialGap.
	self addMessage: messageString centered: false.
	self addGap: 8.
	self addLabels: labels
		images: images
		default: defaultValue
		storeInto: result
		takeKeyboard: true
		equalize: eqBoolean.

	self addGap: 8.
	self addOK: [true].
	self addGap: 8.

	self preOpen.
	builder window maximumSize: builder window displayBox extent.
	builder window minimumSize: builder window displayBox extent.
	builder openDialogWithExtent: builder window displayBox extent.
	^result value!

choose: messageString labels: labels images: images default: defaultValue for: aVisualOrNil
	"Ask the user a question.  Let the user pick from a row of buttons made up
	to match the labels collection.  Return the response from the corresponding
	item from the images collection."

	"aVisualOrNil, if not nil, may be either a VisualPart or a
	ScheduledWindow.  It controls the look and feel and color choices
	used by the dialog, and supplies the dialog's master window, which
	is used by some window systems to create a visual connection between
	the dialog and the window that created it."

	"SimpleDialog new
		choose: 'Are you tired yet?'
		labels: (Array with: 'absolutely' with: 'sort of' with: 'not really')
		images: #(#yes #maybe #no)
		default: #maybe
		for: Dialog defaultParentWindow.
	SimpleDialog new
		choose: 'Are you completely sure?'
		labels: (Array with: 'definitely' with: 'sort of no' with: 'sort of yes' with: 'nope')
		images: #(#yes #maybeNo #maybeYes #no)
		default: #no
		for: Dialog defaultParentWindow"

	^self
		choose: messageString
		labels: labels
		images: images
		default: defaultValue
		equalize: true
		for: aVisualOrNil!

display: messageString fromList: list values: listValues buttons: buttons values: buttonValues  lines: maxLines cancel: cancelBlock  for: aVisualOrNil  initialSelection: theInitialSelection

	| result spec okValue sequence wrappers listW |
	wrappers := OrderedCollection new.
	result := ValueHolder new.
	sequence := SelectionInList new.
	sequence list: list.
	list size = 1
		ifTrue: [sequence selectionIndex: 1]
		ifFalse: [ sequence selectionIndex:  (listValues indexOf: theInitialSelection)].
	spec := (self class interfaceSpecFor: #emptySpec).
	okValue := Object new.
	self initializeBuilderFor: aVisualOrNil.
	builder add: spec window.
	builder add: spec component.
	self initializeWindowFor: aVisualOrNil.

	self setInitialGap.
	self addMessage: messageString centered: false.
	self addGap: 8.
	listW := self
			addList: sequence
			lines: (maxLines min: list size+2)
			validation: [true].
	self addGap: 4.
	wrappers add: (self addOK: [true]).
	buttons isEmpty
		ifFalse:
			[self addGap: 4.
			wrappers add: (self addDivider).
			self addGap: 4.
			wrappers add:
					(self addLabels: buttons
						values: buttonValues
						default: okValue
						storeInto: result
						takeKeyboard: true
						equalize: true)].
	self addGap: 6.
	self bottomAlignLowerEdge: listW.
	self bottomAlign: wrappers.

	(listValues indexOf: theInitialSelection) > 0 ifTrue: [ 
		self postOpenBlock: [:aDialog :aBuilder |    | anIndex  aRange aSequenceView | 
			anIndex := listValues indexOf: theInitialSelection.
			aSequenceView := sequence listHolder dependents detect: [:aE | aE isKindOf: SequenceView] ifNone: [ nil].
			aSequenceView isNil ifFalse: [ 
				aSequenceView targetIndex ~= anIndex ifTrue: [aSequenceView targetIndex: anIndex].
				aSequenceView topComponent displayPendingInvalidation.
				aRange := aSequenceView visibleIntervalForBounds: aSequenceView bounds.
				aRange first = anIndex ifFalse: [
					aSequenceView scrollBy: 0 @ (anIndex - aRange first * aSequenceView lineGrid) negated]]
	 	].
	].
	self preOpen.
	builder openDialogWithExtent: builder window displayBox extent.
	^accept value
		ifTrue: [sequence selectionIndex < 1 ifTrue: [ true] ifFalse: [ listValues at: sequence selectionIndex]]
		ifFalse: [cancel value ifTrue: [cancelBlock value] ifFalse: [result value]]!

display: messageString fromList: list values: listValues lines: maxLines cancel: cancelBlock


	^self display: messageString fromList: list values: listValues buttons: #() values: #()  lines: maxLines cancel: cancelBlock  for: nil  initialSelection: nil! !

CODE_META_Dialogs initializeAfterLoad!

CODE_META_Dialogs loaded!
