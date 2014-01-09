
static void PrintStack(const char *msg) {
	int n;

	printf("PrintStack %s, downwards\n", msg);
	for (n = 0; n < NStackElems; n++) {
		StackElemType *se = &Stack[n];

		printf("Goal: ");
		PrintGoal(&se->GoalField);
		printf("; Progress: RhsUsedField = %d; InpUsedField = %d;\n",
		       se->RhsUsedField, se->InpUsedField);
	}
	printf("\n");
} /* PrintStack */
