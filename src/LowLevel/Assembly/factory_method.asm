; Factory Method — Linux x86-64 / NASM.
; rbx is the replaceable factory-method hook. use_database is invariant.
section .rodata
    pg_connect db "PostgreSQL connect", 10
    pg_connect_len equ $ - pg_connect
    pg_query db "PostgreSQL query", 10
    pg_query_len equ $ - pg_query
    mysql_connect db "MySQL connect", 10
    mysql_connect_len equ $ - mysql_connect
    mysql_query db "MySQL query", 10
    mysql_query_len equ $ - mysql_query

    postgres_database dq print_pg_connect, print_pg_query
    mysql_database dq print_mysql_connect, print_mysql_query

section .text
    global _start

_start:
    lea rbx, [rel create_postgres]
    call use_database
    lea rbx, [rel create_mysql]
    call use_database
    mov rax, 60
    xor rdi, rdi
    syscall

use_database:
    push rbx
    call rbx
    pop rbx
    mov r12, rax
    call [r12]
    call [r12 + 8]
    ret

create_postgres:
    lea rax, [rel postgres_database]
    ret

create_mysql:
    lea rax, [rel mysql_database]
    ret

print_pg_connect:
    lea rsi, [rel pg_connect]
    mov rdx, pg_connect_len
    jmp print

print_pg_query:
    lea rsi, [rel pg_query]
    mov rdx, pg_query_len
    jmp print

print_mysql_connect:
    lea rsi, [rel mysql_connect]
    mov rdx, mysql_connect_len
    jmp print

print_mysql_query:
    lea rsi, [rel mysql_query]
    mov rdx, mysql_query_len
    jmp print

print:
    mov rax, 1
    mov rdi, 1
    syscall
    ret
