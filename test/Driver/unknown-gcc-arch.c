// RUN: %clang -target x86_64-unknown-unknown -c -x assembler %s -### 2>&1 \
// RUN:   | FileCheck -check-prefix=X86_64 %s
// X86_64: {{.*as.*--64}}

// RUN: %clang -target x86_64-unknown-unknown -c -x assembler %s -### -m32 2>&1 \
// RUN:   | FileCheck -check-prefix=X86_64-M32 %s
// X86_64-M32: {{.*as.*--32}}

// RUN: %clang -target i386-unknown-unknown -c -x assembler %s -### 2>&1 \
// RUN:   | FileCheck -check-prefix=I386 %s
// I386: {{.*as.*--32}}

// RUN: %clang -target i386-unknown-unknown -c -x assembler %s -### -m64 2>&1 \
// RUN:   | FileCheck -check-prefix=I386-M64 %s
// I386-M64: {{.*as.*--64}}


// RUN: %clang -target powerpc64-unknown-unknown -c -x assembler %s -### 2>&1 \
// RUN:   | FileCheck -check-prefix=PPC64 %s
// PPC64: {{.*as.*-a64}}

// RUN: %clang -target powerpc64-unknown-unknown -c -x assembler %s -### -m32 2>&1 \
// RUN:   | FileCheck -check-prefix=PPC64-M32 %s
// PPC64-M32: {{.*as.*-a32}}

// RUN: %clang -target powerpc-unknown-unknown -c -x assembler %s -### 2>&1 \
// RUN:   | FileCheck -check-prefix=PPC %s
// PPC: {{.*as.*-a32}}

// RUN: %clang -target powerpc-unknown-unknown -c -x assembler %s -### -m64 2>&1 \
// RUN:   | FileCheck -check-prefix=PPC-M64 %s
// PPC-M64: {{.*as.*-a64}}
