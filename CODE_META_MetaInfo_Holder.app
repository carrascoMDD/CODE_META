'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



((CODE_META createSubApplication: #CODE_META_MetaInfo_Holder in: 'true') isNil)
    ifTrue: [self error: 'Can not proceed since the sub-application was not created']!

CODE_META_MetaInfo_Holder becomeDefault!

Object subclass: #CMMetaInfoPersistencyHolder
	classInstanceVariableNames: 'currentModels '
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_MetaInfo_Holder becomeDefault!

CMMetaInfoPersistencyHolder subclass: #CMDomainRootsMetaInfoPersistencyHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_MetaInfo_Holder becomeDefault!

SubApplication subclass: #CODE_META_MetaInfo_Holder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_MetaInfo_Holder becomeDefault!

TranslationsPersistencyHolder subclass: #CMDomainRootsTranslationPersistencyHolder
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META_MetaInfo_Holder becomeDefault!

!CMDomainRootsMetaInfoPersistencyHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentModel browsePath"
	"self  resetCurrentModels"
	"(self  currentModelStoreMethodSelector: self defaultCurrentModelSelector) browsePath"
	"self  resetCurrentModelStoreMethodSelector: self defaultCurrentModelSelector"! !

!CMDomainRootsMetaInfoPersistencyHolder class publicMethodsFor: 'default'!

defaultCurrentModelSelector
	"self  defaultCurrentTranslationSelector "

	^#domainRootsStore! !

!CMDomainRootsMetaInfoPersistencyHolder class publicMethodsFor: 'modelElements persistence'!

domainRootsStore

	"(CODEElement newFromPersistenceAsCode: CMDomainRootsMetaInfoPersistencyHolder domainRootsStore) browsePath"

	self ojoModel.

	^   #( model 'domainRoots'
	nil nil
	nil
	nil
	CMDomainRootsMetaInfoPersistencyHolder domainRootsStore
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

			 )
			(relationships
			  ( relationship 'homes'
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
				( refToInverseRelationship 'domain'  ( refToType 'Home' 'DomainRootElements'  )  ) 
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
				#'0' #*
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

			 )
			(relationships
			  ( relationship 'domain'
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
				( refToInverseRelationship 'homes'  ( refToType 'Domain' 'DomainRootElements'  )  ) 
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
		  ( type 'Integer'
			nil nil
			nil
			nil
			false false true
			nil nil
			nil
			false false false false
			'0'
			nil
			nil
			nil
			nil
		   )

		  ( type 'CODEElement'
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

		  ( type 'CODEType'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CODEElement' 'PrimitiveTypes'  ) 
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
			nil
			nil
			nil
		   )

		  ( type 'CODEModel'
			nil nil
			nil
			nil
			false false false
			nil nil
			nil
			false false false false
			nil
			(supertypes
			   ( refToType 'CODEElement' 'PrimitiveTypes'  ) 
			 )
			nil
			nil
			nil
		   )

		 )
		nil
	   )

	 )
	CMDomainRootsTranslationPersistencyHolder domainRootsTranslationStore
   )! !

!CMDomainRootsTranslationPersistencyHolder class publicMethodsFor: 'current'!

currentMessages
	"self  currentTranslation browsePath"
	"self  resetCurrentTranslations"
	"(self  currentTranslationStoreMethodSelector: self defaultCurrentTranslationSelector) browsePath"
	"self  resetCurrentTranslationStoreMethodSelector: self defaultCurrentTranslationSelector"
	"self  resetTranslationSelectorsToIgnore"
	"self  translationSelectorsToIgnore"
	"self  translationSelectorsToIgnoreAdd: self defaultCurrentTranslationSelector"! !

!CMDomainRootsTranslationPersistencyHolder class publicMethodsFor: 'default'!

defaultCurrentTranslationSelector
	"CMDomainRootsTranslationPersistencyHolder  defaultCurrentTranslationSelector"

	^#domainRootsNoTranslationStore! !

!CMDomainRootsTranslationPersistencyHolder class publicMethodsFor: 'translations persistence'!

domainRootsNoTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMDomainRootsTranslationPersistencyHolder domainRootsTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'domainRoots_ApplicationTranslation'
	CMDomainRootsTranslationPersistencyHolder domainRootsTranslationStore
	nil
	nil
   )!

domainRootsTranslationStore

	"(TranslationModelBase newFromPersistenceAsCode: CMDomainRootsTranslationPersistencyHolder domainRootsTranslationStore) browsePath"

	self ojoTranslation.

	^   #( application 'domainRoots_ApplicationTranslation'
	CMDomainRootsTranslationPersistencyHolder domainRootsTranslationStore
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
			'Modelo del Dominio'
			nil nil
			nil
		   )

		  ( item 'domainModelCMGO_Attribute_NameOne'
			'domainModelCMGO'
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
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'domainRoots_ApplicationTranslation' ) 
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

		 )
	   )

	  ( group 'PrimitiveTypes::CODEModel_Type'
		nil
		(items
		  ( item 'CODEModel_Type'
			'Modelo%1'
			nil nil
			(usedItems
			   ( refToItemTranslation 'CODEElement_Type' 'PrimitiveTypes::CODEElement_Type' 'domainRoots_ApplicationTranslation' ) 
			 )
		   )

		 )
	   )

	 )
   )! !

!CMMetaInfoPersistencyHolder class publicMethodsFor: 'all'!

resetAllCurrentModels
	"CMMetaInfoPersistencyHolder resetAllCurrentModels"
	(Dialog confirm: 'Do you really want to reset Current Models in all SubClasses ?' initialAnswer: false) ifFalse: [ ^self].

	self resetAllCurrentModelsNoDialog!

resetAllCurrentModelsNoDialog
	"CMMetaInfoPersistencyHolder resetAllCurrentModels"

	self withAllSubclassesDo: [:aTPC | aTPC  resetCurrentModels].! !

!CMMetaInfoPersistencyHolder class publicMethodsFor: 'current'!

currentModel
	"self  currentModel browsePath"

	| aDefaultCurrentModelSelector |

	aDefaultCurrentModelSelector := self defaultCurrentModelSelector.
	aDefaultCurrentModelSelector isNil ifTrue: [ ^nil].

	^self currentModelStoreMethodSelector: aDefaultCurrentModelSelector!

currentModels
	"ModelsPersistencyHolder  currentModels"

	currentModels isNil  ifTrue: [ self resetCurrentModels].
	^currentModels!

currentModelStoreMethodSelector: theStoreMethodSelector
	"self  currentModelStoreMethodSelector: self defaultCurrentModelSelector"
	
	| aModel someCurrentModels |

	aModel := nil.

	someCurrentModels := self currentModels.

	aModel :=  someCurrentModels at:  theStoreMethodSelector ifAbsent: [ nil].

	aModel isNil ifFalse: [ ^aModel].

	aModel := self retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector.
	aModel isNil ifTrue: [ ^nil].

	someCurrentModels at:  theStoreMethodSelector  put: aModel.

	^aModel!

resetCurrentModel
	"self  resetCurrentModel"

	^self resetCurrentModelsStoreMethodSelector: self defaultCurrentModelSelector!

resetCurrentModels
	"self  resetCurrentModels"

	currentModels := IdentityDictionary new.!

resetCurrentModelsStoreMethodSelector: theStoreMethodSelector
	"self resetCurrentModelsStoreMethodSelector: self defaultCurrentModelSelector "
	

	currentModels isNil  ifTrue: [ ^self].

	currentModels removeKey:  theStoreMethodSelector ifAbsent: [ nil]!

retrieveCurrentModelStoreMethodSelector: theStoreMethodSelector
	"self  currentModelStoreMethodSelector: self defaultCurrentModelSelector"
	
	| somePersistenceValues aModel |

	somePersistenceValues := nil.
	Object messageNotUnderstoodSignal 
		handle: [:anEx | ] 
		do: [ somePersistenceValues := self perform:  theStoreMethodSelector].
	(somePersistenceValues isNil or: [ somePersistenceValues isEmpty]) ifTrue: [ ^nil].

 	aModel := CODEElement newFromPersistenceAsCode: somePersistenceValues.
	^aModel! !

!CMMetaInfoPersistencyHolder class publicMethodsFor: 'default'!

defaultCurrentModelSelector
	"self  defaultCurrentTranslationSelector "

	^nil! !

!CMMetaInfoPersistencyHolder class publicMethodsFor: 'ojo'!

ojoMap!

ojoModel! !

CMMetaInfoPersistencyHolder initializeAfterLoad!
CMDomainRootsMetaInfoPersistencyHolder initializeAfterLoad!
CODE_META_MetaInfo_Holder initializeAfterLoad!
CMDomainRootsTranslationPersistencyHolder initializeAfterLoad!

CODE_META_MetaInfo_Holder loaded!
