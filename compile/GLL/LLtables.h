extern void compute_LL_tables(void);
extern int is_nullable(char n);
extern int is_non_terminal(char n);
extern const struct rule **LL_table_entry(char lhs, char look_ahead);



