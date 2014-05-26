#include <stdio.h>

int main(void){
	int kazu, i, j, a;
	printf("素数をいくつまで求めますか？");
	scanf("%d", &kazu );

	for(i = 1; i <= kazu; i++){
		a = 0;
		for(j = 1; j <= i; j++){
			if(i % j == 0){
			a = a + 1;
			}
		}
		if(a==2){
			printf("%d\n",i);
		}

	}

	return 0;
}

