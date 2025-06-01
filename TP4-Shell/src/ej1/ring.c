#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>

int main(int argc, char **argv) {
    // Verificar la cantidad correcta de argumentos
    if (argc != 4) {
        printf("Uso: anillo <n> <c> <s>\n");
        exit(1);
    }

    int n = atoi(argv[1]); // Número de procesos
    int c = atoi(argv[2]); // Valor inicial
    int s = atoi(argv[3]); // Proceso de inicio

    // Validar el número de procesos
    if (n <= 0) {
        printf("El número de procesos tiene que ser positivo\n");
        exit(1);
    }

    // Validar el índice del proceso inicial
    if (s < 0 || s >= n) {
        printf("El proceso de inicio fuera de rango\n");
        exit(1);
    }

    int pipes[n][2];    // Pipes para el anillo
    int padre_env[2];   // Pipe del padre al proceso 's'
    int padre_ret[2];   // Pipe del proceso 's-1' al padre
    pid_t pids[n];      // PIDs de los procesos hijos

    // Crear los pipes del anillo
    for (int i = 0; i < n; i++) {
        if (pipe(pipes[i]) == -1) {
            perror("Error creando pipes del anillo");
            exit(1);
        }
    }
    
    // Crear pipes para comunicarse con el padre
    if (pipe(padre_env) == -1 || pipe(padre_ret) == -1) {
        perror("Error creando pipes de comunicación con el padre");
        exit(1);
    }

    // Crear procesos hijos
    for (int i = 0; i < n; i++) {
        pids[i] = fork();
        
        if (pids[i] == -1) {
            perror("Error en fork");
            exit(1);
        }
        
        // código del hijo
        if (pids[i] == 0) {
            int my_child_idx = i;
            int my_process_num = my_child_idx;
            int read_from_fd;
            int write_to_fd;

            // Determinar de dónde leer y a dónde escribir
            if (my_process_num == s) {
                read_from_fd = padre_env[0];
            } else {
                int prev_child_idx = (my_child_idx - 1 + n) % n;
                read_from_fd = pipes[prev_child_idx][0];
            }

            int process_s_minus_1 = (s == 0) ? (n - 1) : (s - 1);
            if (my_process_num == process_s_minus_1) {
                write_to_fd = padre_ret[1];
            } else {
                write_to_fd = pipes[my_child_idx][1];
            }

            // Cerrar extremos no usados de los pipes del padre
            close(padre_env[1]);
            if (padre_env[0] != read_from_fd) close(padre_env[0]);
            close(padre_ret[0]);
            if (padre_ret[1] != write_to_fd) close(padre_ret[1]);

            // Cerrar extremos no usados del anillo
            for (int j = 0; j < n; j++) {
                if (pipes[j][0] != read_from_fd) close(pipes[j][0]);
                if (pipes[j][1] != write_to_fd) close(pipes[j][1]);
            }
            
            // Leer el valor del pipe de entrada
            int valor;
            if (read(read_from_fd, &valor, sizeof(int)) == -1) {
                perror("Error leyendo del pipe");
                exit(1);
            }
            close(read_from_fd);
            
            // Procesar el valor y mostrar mensaje
            if (my_process_num == s && valor == c) {
                printf("Proceso %d: Enviando valor inicial %d\n", my_process_num, valor + 1);
                valor++;
            } else {
                printf("Proceso %d: Recibido %d, enviando %d\n", my_process_num, valor, valor + 1);
                valor++;
            }
            fflush(stdout);

            // Escribir el valor al siguiente proceso o al padre
            if (write(write_to_fd, &valor, sizeof(int)) == -1) {
                perror("Error escribiendo en el pipe");
                exit(1);
            }
            close(write_to_fd);
            exit(0);
        }
    }

    // Código del proceso padre
    
    // Cerrar los extremos de los pipes del anillo
    for (int i = 0; i < n; i++) {
        close(pipes[i][0]);
        close(pipes[i][1]);
    }

    // Cerrar extremos no usados de los pipes del padre
    close(padre_env[0]);
    close(padre_ret[1]);

    // Enviar valor inicial al proceso 's'
    if (write(padre_env[1], &c, sizeof(int)) == -1) {
        perror("Error enviando valor inicial");
        exit(1);
    }
    close(padre_env[1]);

    // Esperar valor final del proceso 's-1'
    int resultado;
    if (read(padre_ret[0], &resultado, sizeof(int)) == -1) {
        perror("Error recibiendo valor final");
        exit(1);
    }
    close(padre_ret[0]);

    printf("Valor final recibido: %d\n", resultado);

    // Esperar a que terminen todos los hijos
    for (int i = 0; i < n; i++) {
        waitpid(pids[i], NULL, 0);
    }

    return 0;
}