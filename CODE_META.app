'From VisualWorks(R), Release 2.5.2 of September 26, 1995 on April 30, 2018 at 8:31:53 pm'!



Application create: #CODE_META with: 
    (#( CODEgen META PROTO_Translation PROTO_TRF)
        collect: [:each | Smalltalk at: each ifAbsent: [
        self error: 'Not all of the prerequisites are loaded']])!

CODE_META becomeDefault!

Application subclass: #CODE_META
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''!

CODE_META becomeDefault!

CODE_META initializeAfterLoad!

CODE_META loaded!
