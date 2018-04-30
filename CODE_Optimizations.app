'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:58 pm'!



Application create: #CODE_Optimizations with: 
    (#( CODE_META)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

CODE_Optimizations becomeDefault!

Object subclass: #CODEOptimizedMemoryManager
	classInstanceVariableNames: 'memoryManagersDict '
	instanceVariableNames: 'cachedObjectsRegistry cachedObjectsTimes maxObjectMemoryBytes '
	classVariableNames: ''
	poolDictionaries: ''!

CODE_Optimizations becomeDefault!

Application subclass: #CODE_Optimizations
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_Optimizations becomeDefault!

!CODEOptimizedMemoryManager class publicMethodsFor: 'accessing'!

memoryManagerNamed: theName
	| aMemoryManager |
	theName isNil ifTrue: [ ^nil].

	aMemoryManager := self memoryManagersDict at: theName ifAbsent: [ nil].
	aMemoryManager isNil ifTrue: [
		aMemoryManager := self new.
		memoryManagersDict at: theName put: aMemoryManager].

	^aMemoryManager!

memoryManagersDict
	memoryManagersDict isNil ifTrue: [ memoryManagersDict := Dictionary new: 3].
	^memoryManagersDict! !

!CODEOptimizedMemoryManager class publicMethodsFor: 'class initialization'!

initialize
	"self initialize"

	self resetMemoryManagersDict!

reserveEnoughMemory
	"self reserveEnoughMemory"

	| aFreeBytes aToAllocate aRoundedToAllocate |
	ObjectMemory verboseGlobalCompactingGC.
	aFreeBytes := ObjectMemory current availableFreeBytes.
	aFreeBytes < self defaultMaxObjectMemoryBytes ifFalse: [ ^self].

	aToAllocate := self defaultMaxObjectMemoryBytes - aFreeBytes.
	aRoundedToAllocate := (aToAllocate / 1024 / 1024) floor + 1 * 1024 * 1024.
	aRoundedToAllocate < (1024 * 1024 + 1) ifTrue: [ ^self].
	
	ObjectMemory verboseGrowMemoryBy: aRoundedToAllocate!

resetMemoryManagersDict
	memoryManagersDict := nil! !

!CODEOptimizedMemoryManager class publicMethodsFor: 'constants'!

defaultMaxObjectMemoryBytes
	^1024 * 1024 * 40!

defaultMemoryManagerName
	^#CachedObjectsMemoryManager!

initialCachedObjectsRegistrySize

	^5000! !

!CODEOptimizedMemoryManager class publicMethodsFor: 'operations'!

registerCachedObject: theObject
	^self registerCachedObject: theObject inMemoryManagerNamed: self defaultMemoryManagerName!

registerCachedObject: theObject inMemoryManagerNamed: theName
	| aMemoryManager |
	aMemoryManager := self memoryManagerNamed: theName.
	aMemoryManager isNil ifTrue: [ ^nil].

	^aMemoryManager registerCachedObject: theObject!

unregisterCachedObject: theObject
	^self unregisterCachedObject: theObject inMemoryManagerNamed: self defaultMemoryManagerName!

unregisterCachedObject: theObject inMemoryManagerNamed: theName
	| aMemoryManager |
	aMemoryManager := self memoryManagerNamed: theName.
	aMemoryManager isNil ifTrue: [ ^nil].

	^aMemoryManager unregisterCachedObject: theObject! !

!CODEOptimizedMemoryManager publicMethodsFor: 'accessing'!

cachedObjectsRegistry
	cachedObjectsRegistry isNil ifTrue: [ cachedObjectsRegistry := WeakArray new: self initialCachedObjectsRegistrySize].
	^cachedObjectsRegistry!

cachedObjectsTimes
	cachedObjectsTimes isNil ifTrue: [ cachedObjectsTimes := Array new: self initialCachedObjectsRegistrySize].
	^cachedObjectsTimes!

maxObjectMemoryBytes
	maxObjectMemoryBytes isNil ifTrue: [ maxObjectMemoryBytes := self defaultMaxObjectMemoryBytes].
	^maxObjectMemoryBytes!

maxObjectMemoryBytes: theValue
	maxObjectMemoryBytes := theValue! !

!CODEOptimizedMemoryManager publicMethodsFor: 'constants'!

defaultMaxObjectMemoryBytes
	^self class defaultMaxObjectMemoryBytes!

initialCachedObjectsRegistrySize
	^self class initialCachedObjectsRegistrySize! !

!CODEOptimizedMemoryManager publicMethodsFor: 'memory measurement'!

memoryBytesOfObject: theObject
	theObject isNil ifTrue: [ ^0].
	(theObject isKindOf: Object) ifFalse: [ ^0].

	^theObject bitsInstVar size! !

!CODEOptimizedMemoryManager publicMethodsFor: 'operations'!

deallocateFromSumBytes: theSumMemoryBytes forNewBytes: theObjectMemoryBytes atLeastOne: theAtLeastOne

	| aCachedObjectsRegistry aCachedObjectsTimes aMaxObjectMemoryBytes aDeallocatedBytes aTimeNow aToDeallocateMemoryBytes |

	theSumMemoryBytes isNil ifTrue:  [^nil].
	theObjectMemoryBytes isNil ifTrue: [ ^nil].

	aCachedObjectsRegistry := self cachedObjectsRegistry.
	aCachedObjectsRegistry isNil ifTrue: [ ^nil].

	aCachedObjectsTimes := self cachedObjectsTimes.
	aCachedObjectsTimes isNil ifTrue: [ ^nil].

	aMaxObjectMemoryBytes := self maxObjectMemoryBytes.

	((theSumMemoryBytes + theObjectMemoryBytes > aMaxObjectMemoryBytes) not and: [ 
		(theAtLeastOne == true) not]) ifTrue: [ ^nil].

	aDeallocatedBytes := 0.
	aTimeNow := Time now.
	aToDeallocateMemoryBytes := theObjectMemoryBytes.
	[   | anOlderTime anOlderTimeIndex aWasDeallocated anOlderValueHolder anOlderObject anOlderObjectBytes |
		anOlderTime := aTimeNow.
		anOlderTimeIndex := nil.
		aWasDeallocated := false.
		1 to: aCachedObjectsRegistry size do: [:anIndex |  | aValueHolder aValue aTime  |

			aValueHolder := aCachedObjectsRegistry at: anIndex.
			(aValueHolder isNil or: [ aValueHolder == 0])  ifFalse: [ 
				aValue := aValueHolder value.
				aValue isNil ifFalse: [
					aTime := aCachedObjectsTimes at: anIndex.
					aTime isNil ifFalse: [ 
						aTime < anOlderTime ifTrue: [
							anOlderTime := aTime.
							anOlderTimeIndex := anIndex]]]]].

			anOlderTimeIndex isNil ifFalse: [ 		
				anOlderValueHolder := aCachedObjectsRegistry at: anOlderTimeIndex.
				(anOlderValueHolder isNil or: [ anOlderValueHolder == 0]) ifFalse: [ 
					anOlderObject := anOlderValueHolder value.
					anOlderObject isNil ifFalse: [ 
						anOlderValueHolder value: nil.
						anOlderObjectBytes := self memoryBytesOfObject: anOlderObject.
						self reuseObject: anOlderObject.
						aCachedObjectsRegistry at: anOlderTimeIndex put: nil.
						aCachedObjectsTimes at: anOlderTimeIndex put: nil.
						aDeallocatedBytes := aDeallocatedBytes + anOlderObjectBytes.
						aWasDeallocated := true.
Transcript show: '--IM  '; show: anOlderObject extent printString; cr.]]].

		aDeallocatedBytes >= aToDeallocateMemoryBytes ifTrue: [ ^self].
		aWasDeallocated ifFalse: [ ^self].
	] repeat!

freeIndex

	| aCachedObjectsRegistry  |
	
	aCachedObjectsRegistry := self cachedObjectsRegistry.
	aCachedObjectsRegistry isNil ifTrue: [ ^nil].

	1 to: aCachedObjectsRegistry size do: [:anIndex |  | aValueHolder aValue |
		aValueHolder := aCachedObjectsRegistry at: anIndex.
		(aValueHolder isNil or: [ aValueHolder == 0])
			ifTrue: [ ^anIndex]
			ifFalse: [ 
				aValue := aValueHolder value.
				aValue isNil ifTrue: [^anIndex]]].
	
	^nil!

freeIndexAndSumCachedObjectsMemoryBytes

	| aCachedObjectsRegistry aCachedObjectsTimes aFreeIndex  aSumMemoryBytes |


	aCachedObjectsRegistry := self cachedObjectsRegistry.
	aCachedObjectsRegistry isNil ifTrue: [ ^nil].

	aCachedObjectsTimes := self cachedObjectsTimes.
	aCachedObjectsTimes isNil ifTrue: [ ^nil].

	aFreeIndex := nil.
	aSumMemoryBytes := 0.

	1 to: aCachedObjectsRegistry size do: [:anIndex |  | aValueHolder aValue |
		aValueHolder := aCachedObjectsRegistry at: anIndex.
		(aValueHolder isNil or: [ aValueHolder == 0]) 
			ifTrue: [ aFreeIndex isNil ifTrue: [ aFreeIndex := anIndex]]
			ifFalse: [ 
				aValue := aValueHolder value.
				aValue isNil
					ifTrue: [ aFreeIndex isNil ifTrue: [ aFreeIndex := anIndex]]
					ifFalse: [ aSumMemoryBytes := aSumMemoryBytes + (self memoryBytesOfObject: aValue)]]].

	^Array with: aFreeIndex with: aSumMemoryBytes!

registerCachedObject: theValueHolder

	| aCachedObjectsRegistry aCachedObjectsTimes aFreeIndex  aSumMemoryBytes anObjectMemoryBytes anObject aFreeIndexAndSum |


	theValueHolder isNil ifTrue: [ ^nil].
	
	anObject := theValueHolder value.
	anObject isNil ifTrue: [ ^nil].
	
	aCachedObjectsRegistry := self cachedObjectsRegistry.
	aCachedObjectsRegistry isNil ifTrue: [ ^nil].

	aCachedObjectsTimes := self cachedObjectsTimes.
	aCachedObjectsTimes isNil ifTrue: [ ^nil].

	aFreeIndexAndSum := self freeIndexAndSumCachedObjectsMemoryBytes.
	aFreeIndexAndSum isNil ifTrue: [ ^nil].

	aFreeIndex := aFreeIndexAndSum first.
	aSumMemoryBytes := aFreeIndexAndSum at: 2.

	anObjectMemoryBytes :=  self memoryBytesOfObject: anObject.

	self deallocateFromSumBytes: aSumMemoryBytes forNewBytes: anObjectMemoryBytes atLeastOne: aFreeIndex isNil.

	aFreeIndex isNil ifTrue: [  aFreeIndex := self freeIndex].
	aFreeIndex isNil ifTrue: [ ^nil].

	aCachedObjectsRegistry at: aFreeIndex put: theValueHolder.
	aCachedObjectsTimes at: aFreeIndex put: Time now.

Transcript show: '++IM  '; show: anObject extent printString; cr.!

unregisterCachedObject: theValueHolder

	| anObject aCachedObjectsRegistry aCachedObjectsTimes |
	theValueHolder isNil ifTrue: [ ^nil].
	
	anObject := theValueHolder value.


	aCachedObjectsRegistry := self cachedObjectsRegistry.
	aCachedObjectsRegistry isNil ifTrue: [ ^nil].

	aCachedObjectsTimes := self cachedObjectsTimes.
	aCachedObjectsTimes isNil ifTrue: [ ^nil].


	1 to: aCachedObjectsRegistry size do: [:anIndex |  | |
		(aCachedObjectsRegistry at: anIndex) == theValueHolder ifTrue: [ 
			theValueHolder value: nil.
			anObject isNil ifFalse: [ 
				self reuseObject: anObject.
				aCachedObjectsRegistry at: anIndex put: nil.
				aCachedObjectsTimes at: anIndex put: nil.
Transcript show: '--IM  '; show: anObject extent printString; cr].
			^self]]! !

!CODEOptimizedMemoryManager publicMethodsFor: 'reuse'!

reuseObject: theObject
	| aByteArray |
	theObject isNil ifTrue: [ ^nil].

	aByteArray := theObject bitsInstVar.
	aByteArray isNil ifFalse: [ aByteArray reuseByteArray].

	theObject bits: nil! !

CODEOptimizedMemoryManager initializeAfterLoad!
CODE_Optimizations initializeAfterLoad!

CODE_Optimizations loaded!
