global _start

section .data
readme: dq 0, 2, 0, 0
api: dq 0, 3, 0, 0
guide: dq 0, 5, 0, 0

docs_children: dq api, guide
docs: dq 1, 0, docs_children, 2

root_children: dq readme, docs
root: dq 1, 0, root_children, 2

section .rodata
output: db "leaf=2", 10, "docs=8", 10, "root=10", 10
output_len equ $ - output

section .text

; Node layout: kind, bytes, children pointer, child count.
; size_node accepts a Node* in RDI and returns its recursive size in RAX.
size_node:
    push rbx
    push r12
    push r13
    push r14

    mov r12, rdi
    cmp qword [r12], 0
    jne .folder

    mov rax, [r12 + 8]
    jmp .done

.folder:
    xor rbx, rbx
    xor r13, r13
    mov r14, [r12 + 16]

.loop:
    cmp r13, [r12 + 24]
    jae .sum
    mov rdi, [r14 + r13 * 8]
    call size_node
    add rbx, rax
    inc r13
    jmp .loop

.sum:
    mov rax, rbx

.done:
    pop r14
    pop r13
    pop r12
    pop rbx
    ret

_start:
    mov rdi, readme
    call size_node
    cmp rax, 2
    jne .fail

    mov rdi, docs
    call size_node
    cmp rax, 8
    jne .fail

    mov rdi, root
    call size_node
    cmp rax, 10
    jne .fail

    mov rax, 1
    mov rdi, 1
    mov rsi, output
    mov rdx, output_len
    syscall

    mov rax, 60
    xor rdi, rdi
    syscall

.fail:
    mov rax, 60
    mov rdi, 1
    syscall
