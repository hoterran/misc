# define __CPU_SETSIZE  1024
# define __NCPUBITS (8 * sizeof (__cpu_mask))
 
 /* Type for array elements in 'cpu_set'.  */
typedef unsigned long int __cpu_mask;
  
typedef struct
{
    __cpu_mask __bits[__CPU_SETSIZE / __NCPUBITS];
} cpu_set_t;

int main() {
    printf("cpu mask %d\n",  sizeof(__cpu_mask));
    printf("cpu_set_t %d\n",  sizeof(cpu_set_t));
}
