/*
*  Null model for paper: 'Reciprocity and success of academic careers', by
*  Weihua Li, Tomaso Aste, Fabio Caccioli & Giacomo Livan.
*  
*	 Null model #1: Change the citation of papers to see what happens to the 
*  reciprocity of the author citation network. It imposes restrictions on
*  the publication time ordering and author network community structures.
*  Each author is given a membership to a community detected by the modularity-
*  based community algorithm by 'Clauset A, Newman M E J, Moore C. Finding 
*  community structure in very large networks. Physical Review E, 2004, 70(6): 066111.
*  
*	 The null model works as follows: Suppose we have two original citations,
*	 paper A -> paper E,
*	 paper C -> paper F; 
*	 Two restrictions must be met for a rewiring:
*	 (1) The publication time of paper F is earlier than paper A,
*	 and the publication time of paper E is earlier than paper C;
*	 (2) We can find an author a of paper A and author c of paper C such that
*	 they belong to the same community, and an author e of paper E and author 
*	 f of paper F such that they belong to the same community.
*	 After the rewiring the citations would be:
*	 paper A -> paper F,
*	 paper C -> paper E.
*	 
*	 Please use the citNet_dataPrep.R to prepare data first and then run
*	 this null model in C.
*	 
*/

#include "fstream"
#include "iostream"
#include <string>
#include <sstream>
#include <limits>
#include <queue>
#include <vector>
#include <time.h>
#include "math.h"
#include <algorithm>
using namespace std;

#define N_AUTHOR 3644   				// max author id + 1;
#define N_PAPER  21690  				// max paper id + 1
#define N 151618  				// total number of paper citations + 1

long long int numrewire = 1000000000;
int yearst = 1893;

char filename[100];
int module[N_AUTHOR] = {0};  
vector<int> paper_author[N];
int ni_paper[N] = {0}, nj_paper[N] = {0}; // PAPER citations, ni_paper[k] cites nj_paper[k], nj_paper fixed and swaps will change ni_paper; nj can be indexed by idxst_paper, idxend_paper
int year[N_PAPER] = {0};	// publication year of paper

bool condition(int A, int C, int E, int F);
void check_initial_authorcit();
void input();
bool paper_module(int pA, int pF);

//  see if we can find author a from paper pA and author f from
//  paper pF, such that a and f are in the same community
bool paper_module(int pA, int pF){ 
	bool judge = 0;
	vector<int> moduleA1, moduleF1;  
	int i, j;

	for(i = 0; i < paper_author[pA].size(); i++){
		moduleA1.push_back(module[paper_author[pA][i]]);
	}
	for(i = 0; i < paper_author[pF].size(); i++){
		moduleF1.push_back(module[paper_author[pF][i]]);
	}
	unique(moduleA1.begin(), moduleA1.end());
	unique(moduleF1.begin(), moduleF1.end());
	sort(moduleA1.begin(), moduleA1.end()); 
	sort(moduleF1.begin(), moduleF1.end());

	// then find if there is a pair of authors in the same community; 
	i = 0; j = 0; 
	while(i < moduleA1.size() && j < moduleF1.size()){
		if(moduleA1[i] == moduleF1[j]){
			judge = 1;
			break;
		} else if(moduleA1[i] > moduleF1[j]){
			j++;
		} else{
			i++;
		}
	}
	return judge;
}

// see if we can rewire citations pA -> pF && pC -> pE
bool condition(int pA, int pC, int pE, int pF){ 
	bool judge = 1;
	if(pA == pF || pC == pE )  // citing and cited papers cannot be the same
		return 0;
	else if(pE == pF || pA == pC) 
		return 0;
	else if(year[pA] < year[pF] || year[pC] < year[pE]) // publication time A, C >= E, F
		return 0;
	else if(!paper_module(pA, pC)) return 0;  // author_cit network keep module
	else if(!paper_module(pE, pF)) return 0;
	return judge;
}

void input(){
	ifstream fin;

	// Read in paper citations
	fin.open("paper_cit.dat");
	int citing_paper, cited_paper, count = 1; 
	ni_paper[0] = -1; 
	nj_paper[0] = -1;
	while(fin >> citing_paper >> cited_paper){
		if(count % 1000000 == 0) cout << count <<endl; 
		ni_paper[count] = citing_paper;
		nj_paper[count] = cited_paper;
		count++;
	}
	fin.close();

	//  id community module
	fin.open("id_module.dat");	
	int author, mod;	
	while(fin >> author >> mod){
		module[author] = mod;
	}
	fin.close();

	// read in author -- paper data
	fin.open("paper_author.dat");	
	int paper;
	while(fin >> paper >> author){
		paper_author[paper].push_back(author);
	}
	fin.close();

	// publication year
	fin.open("paper_year.txt");
	int yr;
	while(fin >> paper >> yr){
		year[paper] = yr;
	}
	fin.close();
	
	cout << "Data read" << endl;
	return;
}

int main(){
	srand((time(NULL) + 1));
	input();
	ofstream fout_recp, fout;
	long long int  idx1, idx2;
	int  file = 1, pA, pC, pE, pF;
	long long int i, j, k, tmp;
	double p = 0.5, rnd;
	char filename[100];

	cout << "HELLO" <<endl;
	for(i = 1; i <= numrewire; i++){
		if(i % 100000000 == 0){
			cout << "swap " << i << endl;
		  
		  //  output a snapshot of the null model
		  sprintf(filename, "%s%d%s", "Citations_null_model_", file, "_.out");
		  file++;
		  fout.open(filename);
		  for(j = 1; j < N; j++){ 
		    fout << ni_paper[j] << ' ' << nj_paper[j] << endl;
		  }
		  fout.close();
		}
		
		//  randomly select two citations
		idx1 = rand() % N + 1;
		idx2 = rand() % N + 1;

		if(idx1 == idx2) continue;
		else{
			// pA is the paper id of paper A
			pA = ni_paper[idx1];	
			pE = nj_paper[idx1];	
		  pC = ni_paper[idx2];				
			pF = nj_paper[idx2];
		}
		if(condition(pA, pC, pE, pF)){	
		   // if and only if these papers are distinct
			 // swap citation of papers;
			 // A -> F, C -> E
			 // first swap citation of authors
			 tmp = ni_paper[idx1];
			 ni_paper[idx1] = ni_paper[idx2];
			 ni_paper[idx2] = tmp;
		}
	}

	return 1;
}
