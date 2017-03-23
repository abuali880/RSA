#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <math.h>
#include <fstream>
#include <cstdio>

using namespace std;

unsigned GetNumberOfDigits (long long i)
{
    return i > 0 ? (int) log10 ((double) i) + 1 : 1;
}

vector<long long> ConvToArray(string P)
{
    vector<long long> p;
    unsigned int ni=0;
    string tmp = "";
    p.resize((P.length()/9)+2);
    for(unsigned int i = 0;i<(P.length()/9);i++)
    {
        for(unsigned int k = ni ; k<ni+9 ; k++)
        {
            tmp+=P[P.size() - 1 - k];
        }
        reverse(tmp.begin(),tmp.end());
        std::stringstream ss(tmp);
        ss >> p[i];
        tmp="";
        ni=ni+9;
    }
    for(unsigned int k = ni ; k<P.size() ; k++)
    {
        tmp+=P[P.size() - 1 - k];
    }
    reverse(tmp.begin(),tmp.end());
    std::stringstream ss(tmp);
    ss >> p[p.size()-2];
    for(int i = p.size()-2 ; i > 0 ; i--)
    {
        if(p[i] == 0)
        {
            p.erase(p.end()-1);
        }
        else break;
    }
    return p;
}

string ConvToString(vector<long long> tar)
{
    string P="";
    int co=0;
    int l = tar.size();
    for(int i = l-2 ; i >= 0 ; i--)
    {
        if(i!=l-2)
        {
            co = GetNumberOfDigits(tar[i]) ;
            if(co<9)
            {
                for(int k = 0; k < 9-co; k++)
                {
                    P+="0";
                }
            }
        }

        P+= to_string(tar[i]);
    }
    return P;
}

class BigNumber
{
public:
    vector<long long>num;
    BigNumber()
    {
        num.clear();
    }
    BigNumber(string P)
    {
        num = ConvToArray(P);
    }
    void SetStr(string P)
    {
        num = ConvToArray(P);
    }

    bool IsEqual(BigNumber b)
    {
        if(num.size() != b.num.size()) return false;
        else
        {
            for(int i = 0 ; i < int(num.size())-1;i++)
            {
                if(num[i] != b.num[i]) return false;
            }
        }
        return true;
    }

    bool isGreaterBig(BigNumber b)
    {
        if(num.size() > b.num.size()) return true;
        else if (num.size() == b.num.size())
        {
            for(int i = num.size()-2 ; i >= 0 ; i--)
            {
                if(num[i] > b.num[i])
                {
                    return true;
                }
                else if(num[i] < b.num[i])
                {
                    break;
                }
            }
        }
        return false;
    }
    BigNumber sum(BigNumber b)
    {
        BigNumber res,tm=*this;
        int l;
        if(num.size() > b.num.size())
        {
            l = num.size();
            b.num.resize(l);
        }
        else
        {
            l = b.num.size();
            num.resize(l);
        }
        res.num.resize(l+1);
        for(int i = 0 ; i < l ; i++)
        {
            long long r;
            r=num[i]+b.num[i];
            if(r+res.num[i]>999999999)
            {
                res.num[i]+=r-1000000000;
                res.num[i+1]++;
            }
            else
            {
                res.num[i] +=r;
            }
        }
        for(int i = l-1 ; i > 0 ; i--)
        {
            if(res.num[i] == 0)
            {
                res.num.erase(res.num.end()-1);
            }
            else break;
        }
        *this = tm;
        return res;
    }
    BigNumber sub(BigNumber b)
    {
        int l;
        BigNumber res,tm=*this;
        if(num[num.size()-1] == -1 && b.num[b.num.size()-1] == -1)
        {
            num[num.size()-1] = 0;
            b.num[b.num.size()-1] = 0;
            return b.sub(*this);
        }
        else if(b.num[b.num.size()-1] == -1)
        {
            b.num[b.num.size()-1]=0;
            return this->sum(b);
        }
        else if(num[num.size()-1] == -1)
        {
            num[num.size()-1] = 0;
            res = this->sum(b);
            res.num[res.num.size()-1] = -1;
            return res;
        }
        if(this->isGreaterBig(b))
        {
            l = num.size();
            res.num.resize(l);
            b.num.resize(l);
        }
        else if(b.isGreaterBig(*this))
        {
            vector<long long> temp;
            l = b.num.size();
            res.num.resize(l);
            num.resize(l);
            temp = b.num;
            b.num = num;
            num = temp;
            res.num[res.num.size()-1] = -1;
        }
        else
        {
            res.num.resize(2);
            return res;
        }
        for(int i = 0 ; i < l-1 ; i++)
        {
            long long r;
            r=num[i]-b.num[i];
            if(r-res.num[i]<0)
            {
                res.num[i]+=r+1000000000;
                num[i+1]--;
            }
            else
            {
                res.num[i] +=r;
            }
        }
        for(int i = l-2 ; i > 0 ; i--)
        {
            if(res.num[i] == 0)
            {
                res.num.erase(res.num.end()-1);
            }
            else break;
        }
        *this = tm;
        return res;
    }
    BigNumber mul(BigNumber b)
    {
        BigNumber res,tm=*this;
        res.num.resize(num.size()+b.num.size()-1);
        int l = 0;
        unsigned k;
        long long tempo,tempo1,carry=0,r5ama;
        for(unsigned i = 0 ; i < b.num.size()-1; i++)
        {
            for(k = 0 ; k < num.size()-1; k++)
            {
                tempo = b.num[i] * num[k];
                tempo+=carry;
                //tempo1 = tempo %1000000000;
                res.num[k+l]+= tempo %1000000000;
                //tempo1 = tempo - tempo1;
                carry= tempo/1000000000;
                if(res.num[k+l]> 999999999)
                {
                    r5ama = res.num[k+l];
                    res.num[k+l] = res.num[k+l]%1000000000;
                    res.num[k+l+1] += (r5ama - res.num[k+l])/1000000000;
                }

            }
            res.num[k+l]+=carry;
            carry=0;
            l++;
        }
        l= res.num.size();
        for(int i = l-2 ; i > 0 ; i--)
        {
            if(res.num[i] == 0)
            {
                res.num.erase(res.num.end()-1);
            }
            else break;
        }
        if((num[num.size() - 1] == -1 && b.num[b.num.size() - 1]==0) || (num[num.size() - 1] == 0 && b.num[b.num.size() - 1]==-1)) res.num[res.num.size()-1] = -1;
        *this = tm;
        return res;
    }
    BigNumber div2()
    {
        BigNumber res;
        int l = num.size()-2;
        res.num.resize(l+2);
        long long carry = 0;
        for(int i = l ; i >= 0 ; i--)
        {
            res.num[i] = (num[i]+carry)/2;
            if(num[i]%2!=0)
            {
                carry = 1000000000;
            }
            else carry = 0;
        }
        l = res.num.size()-2;
        for(int i = l ; i > 0 ; i--)
        {
            if(res.num[i] == 0)
            {
                res.num.erase(res.num.end()-1);
            }
            else break;
        }
        return res;
    }

    BigNumber ChooseBest(vector<BigNumber>choices)
    {
        BigNumber tmp,res,TmpFir,t;
        int num1,index=-1,Ba2i=0,k;
        string Fir="",Ba2iFir="",Res="";
        bool flag=true,flag2=false;
        res.num.resize(0);
        Fir = ConvToString(num);
        re:;
        for(k = 0 ; k < 4 ; k++)
        {
            tmp.num = choices[k].num;
            num1 = GetNumberOfDigits(tmp.num[tmp.num.size()-2]) + (9*(tmp.num.size()-2));
            if(flag2)
                num1++;
            TmpFir.num = ConvToArray(Fir.substr(0,num1));
            BigNumber us = TmpFir.sub(tmp);
            if((TmpFir.isGreaterBig(tmp) || TmpFir.IsEqual(tmp)) && flag)
            {
                t = TmpFir.sub(tmp);
                res.num=choices[k].num;
                index = k;
                flag = false;
            }
            else if(t.isGreaterBig(us) && (TmpFir.isGreaterBig(tmp)|| TmpFir.IsEqual(tmp)))
            {
                res.num=choices[k].num;
                index = k;
                t = TmpFir.sub(tmp);
            }
        }
        if(res.num.size()==0)
        {
            flag2=true;
            goto re;
        }
        num1 = GetNumberOfDigits(res.num[res.num.size()-2]) + (9*(res.num.size()-2));
        if(flag2)
            num1++;
        if(int(Fir.length()) < num1) num1=Fir.length();
        Ba2i = Fir.length() - num1;
        Ba2iFir = Fir.substr(num1,Ba2i);
        Fir = ConvToString(t.num) + Ba2iFir;
        num = ConvToArray(Fir);
        if(index == 0) index = 1;
        else if(index == 1) index = 2;
        else if(index == 2) index = 4;
        else if(index == 3) index = 8;
        Res = to_string(index);
        for(int j = 0 ; j < Ba2i ; j++) Res+="0";
        res.num = ConvToArray(Res);
        return res;
    }
    BigNumber divM(BigNumber b,int mode=0)
    {
        BigNumber two,res,tmp;
        if(b.num.size() == 2)
        {
            if(num.size()==2)
            {
                res.num.resize(2);
                if(mode==0)
                    res.num[0]=num[0]/b.num[0];
                else
                    res.num[0]=num[0]%b.num[0];
                return res;
            }
            if(b.num[0]==2)
            {
                res = this->div2();
                if(mode==0) return res;
                else
                {
                    res.num.clear();
                    res.num.resize(2);
                    if(num[0]%2!=0)
                    {
                        res.num[0]=1;
                    }
                    return res;
                }
            }
        }
        tmp.num = num;
        vector<BigNumber>choices;
        res.num.resize(2);
        two.num.resize(2);
        two.num[0]=2;
        choices.resize(4);
        choices[0]=b;
        choices[1]=b.mul(two);
        choices[2]=choices[1].mul(two);
        choices[3]=choices[2].mul(two);
        while(this->isGreaterBig(b) || this->IsEqual(b))
        {
            res = this->ChooseBest(choices).sum(res);
        }

        if(mode==0)
        {
            num = tmp.num;
            return res;
        }
        else
        {
            vector<long long>tmp2 = tmp.num;
            tmp.num = num;
            num = tmp2;
            return tmp;
        }
    }

    void rem(BigNumber mod, BigNumber & use, BigNumber & ten , BigNumber & remainder)
    {
        if(mod.isGreaterBig(*this))
        {
            remainder = *this;
            return;
        }
        else if(this->IsEqual(mod))
        {
            remainder.num.clear();
            remainder.num.resize(2);
            return;
        }
        string P = ConvToString(this->num);
        remainder.num.clear();
        remainder.num.resize(2);
        use.num.clear();
        use.num.resize(2);
        for (int i = 0; P[i] != '\0'; ++i)
        {
            remainder = remainder.mul(ten);
            use.num[0] = P[i]-'0';
            remainder = remainder.sum(use);
            while(remainder.isGreaterBig(mod) ||remainder.IsEqual(mod))
                remainder = remainder.sub(mod);
        }
    }

    void powe(BigNumber b,BigNumber mod,BigNumber&res,BigNumber & use, BigNumber & ten)
    {
        if(b.num[0]==2)
        {
            res = this->mul(*this);
            res.rem(mod,use,ten,res);
            return ;
        }
        if(b.num[0]%2 != 0)
        {
            b.num[0]--;
            this->powe(b,mod,res,use,ten);
            res.mul(*this).rem(mod,use,ten,res);
        }
        else
        {
            b = b.div2();
            this->powe(b,mod,res,use,ten);
            res.mul(res).rem(mod,use,ten,res);
        }
        return ;
    }
    bool IsEqualZero()
    {
        if(num.size() == 2)
        {
            if(num[0]==0) return true;
            else return false;
        }
        else
        {
            return false;
        }
    }
    bool IsEqualOne()
    {
        if(num.size() == 2)
        {
            if(num[0]==1) return true;
            else return false;
        }
        else
        {
            return false;
        }
    }
    bool isPrime()
    {

        if(num.size()==2)
        {
            if(num[0]==2 || num[0]==3 || num[0]==5 || num[0]==7) return true;
            else if(num[0]==1 || num[0]==4) return false;
        }
        if(num[0]%2 == 0) return false;
        BigNumber q,res,a,tmp,tmp2,use,ten("10");
        q.num=num;
        q.num[0]--;
        a.num.resize(2);
        tmp2.num.resize(2);
        long long k=0;
        int tr=0;
        while(q.num[0]%2==0)
        {
            q = q.div2();
            k++;
        }
        a.num[0]=2;
        if(q.num.size()==2 && q.num[0]==1) res = a;
        else a.powe(q,*this,res,use,ten);
        if(res.num.size()==2)
        {
            if(res.num[0]==1)
            {
                tr++;
                goto fir;
            }
        }
        tmp=*this;
        tmp.num[0]--;
        if(res.IsEqual(tmp))
        {
            tr++;
            goto fir;
        }
        for(int i = 1 ; i <= k-1;i++)
        {
            res.mul(res).rem(*this,use,ten,res);
            if(res.IsEqual(tmp))
            {
                tr++;
                goto fir;
            }
        }
        fir:
//        a.num[0]=3;
//        if(q.num.size()==2 && q.num[0]==1) res = a;
//        else a.powe(q,*this,res,use,ten);
//        if(res.num.size()==2)
//        {
//            if(res.num[0]==1)
//            {
//                tr++;
//                goto sec;
//            }
//        }
//        tmp=*this;
//        tmp.num[0]--;
//        if(res.IsEqual(tmp))
//        {
//            tr++;
//            goto sec;
//        }
//        for(int i = 1 ; i <= k-1;i++)
//        {
//            res.mul(res).rem(*this,use,ten,res);
//            if(res.IsEqual(tmp))
//            {
//                tr++;
//                goto sec;
//            }
//        }
//        sec:
        //  if a third iteration is needed
//        a.num[0]=5;
//        if(q.num.size()==2 && q.num[0]==1) res = a;
//        else a.powe(q,*this,res,use,ten);
//        if(res.num.size()==2)
//        {
//            if(res.num[0]==1)
//            {
//                tr++;
//                goto th;
//            }
//        }
//        tmp=*this;
//        tmp.num[0]--;
//        if(res.IsEqual(tmp))
//        {
//            tr++;
//            goto th;
//        }
//        for(int i = 1 ; i <= k-1;i++)
//        {
//            res.mul(res).rem(*this,use,ten,res);
//            if(res.IsEqual(tmp))
//            {
//                tr++;
//                goto th;
//            }
//        }
//        th:
        if(tr==1) return true;
        return false;
    }

    BigNumber ExtEc(BigNumber mod)
    {
        BigNumber A2,A3,B2,B3,Q,T2,T3;
        A2.num.resize(2);
        A3 = mod;
        B2.num.resize(2);
        B2.num[0]=1;
        B3 = *this;
        while(!B3.IsEqualZero() && !B3.IsEqualOne())
        {
            Q = A3.divM(B3);
            T2 = Q.mul(B2);
            T2 = A2.sub(T2);
            T3 = Q.mul(B3);
            T3 = A3.sub(T3);
            A2 = B2;
            A3 = B3;
            B2 = T2;
            B3 = T3;
        }
        if(B3.IsEqualOne())
        {

            while(B2.num[B2.num.size()-1]==-1)
            {
                B2.num[B2.num.size()-1] = 0;
                B2 = mod.sub(B2);
            }
            return B2;
        }
        else
        {
            BigNumber no;
            no.num.resize(2);
            no.num[0]=-1;
            return no;
        }
    }

};



int main()
{
     FILE * f = new FILE() ;
     f = freopen("/home/mostafa/Qt/Projects/SecurityProject/i.txt","r",stdin);
    string P,Q,E,choice="";
    cin >> P >> Q >> E;
    bool flag = false,flag1=false;
    if (P[0] == 'P')
        P = P.substr(2,P.size()-1);
    if (Q[0] == 'Q')
        Q = Q.substr(2,Q.size()-1);
    if (E[0] == 'E')
        E = E.substr(2,E.size()-1);
    BigNumber * p1 = new BigNumber(P);
    BigNumber * q1= new BigNumber(Q);
    BigNumber * e1=new BigNumber(E);
    BigNumber *n1= new BigNumber();
    BigNumber *d1= new BigNumber();
    BigNumber *PhiN1= new BigNumber();
    BigNumber *one1= new BigNumber("1");
    BigNumber *c1= new BigNumber();
    BigNumber *m1= new BigNumber();
    BigNumber *use = new BigNumber();
    BigNumber *ten = new BigNumber();
    ten->num.resize(2);
    ten->num[0] = 10;
    *n1 = p1->mul(*q1);
    *PhiN1 = p1->sub(*one1).mul(q1->sub(*one1));
    *d1 = e1->ExtEc(*PhiN1);

    while(true)
    {
        cin >> choice;
        if(choice == "IsPPrime")
        {
            flag = p1->isPrime();
            if(flag) cout << "Yes\n";
            else cout << "No\n";
        }
        else if(choice == "IsQPrime")
        {
            flag1 = q1->isPrime();
            if(flag1) cout << "Yes\n";
            else cout << "No\n";
        }
        else if(choice == "PrintN")
        {
            cout << ConvToString(n1->num) + "\n";
        }
        else if(choice == "PrintPhi")
        {
            cout << ConvToString(PhiN1->num) + "\n";
        }
        else if(choice == "PrintD")
        {
            cout << ConvToString(d1->num) + "\n";
        }
        else if(choice.substr(0,15) =="EncryptPublic=<")
        {
            choice = choice.substr(15,choice.length()-1);
            choice.pop_back();
            m1->SetStr(choice);
            m1->powe(*e1,*n1,*c1,*use,*ten);
            cout << ConvToString(c1->num) + "\n";
        }
        else if(choice.substr(0,16) =="EncryptPrivate=<")
        {
            choice = choice.substr(16,choice.size()-1);
            choice.pop_back();
            m1->SetStr(choice);
            m1->powe(*d1,*n1,*c1,*use,*ten);
            cout << ConvToString(c1->num) + "\n";
        }
        else if(choice == "Quit") break;
    }
    return 0;
}
