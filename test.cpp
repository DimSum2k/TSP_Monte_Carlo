#include <cstdlib>
#include <ctime>
#include <iostream>

// Creer des classes, plus efficace pour le faire


using namespace std;


double doubleRand() {
    return double(rand()) / (double(RAND_MAX) + 1.0);
}


void generate_u(double tableau[], int length) {
    srand(static_cast<unsigned int>(clock()));
    cout << "expect " << length <<  " numbers within the interval [0.0, 1.0)" << endl;
    for (int i=0; i < length; i++) {
        tableau[i] = doubleRand();
    }
    cout << endl;
}

int main() {
    
    clock_t begin = clock(); //to measure execution time
    
    int n = 10000000; //Probleme de tailler, comment l'ameliorer ?
    double u[n];
    generate_u(u, n);
    //for(int i(0); i<n; ++i) {
    //   cout << u[i] << endl;
    //}
    
    float prob[4] = {float(4)/12,float(1)/12,float(5)/12,float(2)/12};
    for(int i(0); i<4; ++i) {
        cout << "Theoritical Probability : " << prob[i] << endl;
    }
    int output[4];
    
    for (int el=0; el < n; el++){
        float sum=0;
        for (int p=0; p < 4; p++) {
            sum += prob[p];
            if (u[el] <= sum) {output[p]+=1; break;}
        }
    }
    for(int i(0); i<4; ++i) {
        cout << "Theoritical Probability : " << prob[i] << endl;
        cout << "Frequency observed : " << output[i]/float(n) << endl;
    }
    
    clock_t end = clock();
    double elapsed_secs = double(end - begin) / CLOCKS_PER_SEC;
    cout << "Execution Time : " << elapsed_secs << " sec " << endl;
    
}




         
         

