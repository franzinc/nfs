
(in-package :comp)

(def-i386-instr bswap-eax iq-asm-comment nil
		(iq-ia-hex nil #x0f #xc8))

(def-i386-instr bswap-ecx iq-asm-comment nil
		(iq-ia-hex nil #x0f #xc9))

(def-i386-instr bswap-ebx iq-asm-comment nil
		(iq-ia-hex nil #x0f #xcb))

(def-i386-instr bswap-edx iq-asm-comment nil
		(iq-ia-hex nil #x0f #xca))




