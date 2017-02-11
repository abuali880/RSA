#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <math.h>
#include <fstream>

using namespace std;

//ofstream myfile("/home/mostafa/Qt/Projects/SecurityProject/testright.txt");

unsigned GetNumberOfDigits (long long i)
{
    return i > 0 ? (int) log10 ((double) i) + 1 : 1;
}

vector<long long> ConvToArray(string P)
{
    vector<long long> p;
    unsigned int pL;
    unsigned int ni=0;
    string tmp = "";
    pL = (P.length()/9)+1;
    p.resize(pL+1);
    for(unsigned int i = 0;i<pL-1;i++)
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

    bool IsEqual(BigNumber b)
    {
        if(num.size() != b.num.size()) return false;
        else
        {
            int l = num.size();
            for(int i = 0 ; i < l-1;i++)
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
        res.num.resize(l);
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
                tempo1 = tempo %1000000000;
                res.num[k+l]+= tempo1;
                tempo1 = tempo - tempo1;
                carry= (tempo1)/1000000000;
                if(k==0 || i==num.size()-2)
                {
                    if(res.num[k+l]> 999999999)
                    {
                        r5ama = res.num[k+l];
                        res.num[k+l] = res.num[k+l]%1000000000;
                        res.num[k+l+1] += (r5ama - res.num[k+l])/1000000000;
                    }
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

    BigNumber rem(BigNumber mod)
    {
        if(mod.isGreaterBig(*this))
        {
            return *this;
        }
        else if(this->IsEqual(mod))
        {
            BigNumber r;
            r.num.resize(0);
            return r;
        }
        BigNumber remainder,tmpo1,tmp;
        remainder.num.resize(1);
        tmpo1.num.resize(2);
        for(int i = this->num.size()-2; i >= 0; i--)
        {
            remainder.num.insert(remainder.num.begin(),0);
            tmpo1.num[0] = this->num[i];
            tmp = remainder.sum(tmpo1);
            if(tmp.isGreaterBig(mod)|| tmp.IsEqual(mod))
            {
                remainder = tmp.divM(mod,1);
            }
            else remainder = tmp;
        }
        return remainder;
    }

    void powe(BigNumber b,BigNumber mod,BigNumber&res)
    {
        if(b.num[0]==2)
        {
            res = this->mul(*this);
            //res = res.divM(mod,1);
            res = res.rem(mod);
            return ;
        }
        if(b.num[0]%2 != 0)
        {
            b.num[0]--;
            this->powe(b,mod,res);
            //res = y.divM(mod,1);
            res = res.mul(*this).rem(mod);
        }
        else
        {
            b = b.div2();
            this->powe(b,mod,res);
            //res = y.divM(mod,1);
            res = res.mul(res).rem(mod);
        }
        return ;//res;
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
        if(num[0]%2 == 0) return false;
        if(num.size()==2)
        {
            if(num[0]==2 || num[0]==3) return true;
            else if(num[0]==1 || num[0]==4) return false;
        }
        BigNumber q,res,a,tmp,tmp2;
        q.num=num;
        q.num[0]--;
        a.num.resize(2);
        tmp2.num.resize(2);
        long long k=0;
        while(q.num[0]%2==0)
        {
            q = q.div2();
            k++;
        }
        a.num[0]=2;
        a.powe(q,*this,res);
        if(res.num.size()==2)
        {
            if(res.num[0]==1) return true;
        }
        tmp=*this;
        tmp.num[0]--;
        if(res.IsEqual(tmp)) return true;
        for(int i = 1 ; i <= k-1;i++)
        {
            res = res.mul(res).divM(*this,1);
            if(res.IsEqual(tmp)) return true;
        }
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
        cout << ConvToString(B2.num);
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



bool IsEqual(vector<long long>fir,vector<long long>sec)
{
    if(fir.size() != sec.size()) return false;
    else
    {
        int l = fir.size();
        for(int i = 0 ; i < l-1;i++)
        {
            if(fir[i] != sec [i]) return false;
        }
    }
    return true;
}

//vector<long long> LeftAssio(vector<long long> tar)
//{
//    string P="";
//    for(int i = tar.size()-2 ; i >= 0 ; i--)
//    {
//        if(i!=tar.size()-2)
//        for(int k = 0; k < 9-GetNumberOfDigits(tar[i]) ; k++) P+="0";
//        P+= to_string(tar[i]);
//    }
//    vector<long long> p;
//    unsigned int pL;
//    unsigned int ni=0;
//    string tmp = "";
//    pL = (P.length()/9)+1;
//    p.resize(pL+1);
//    unsigned int i ;
//    for(i = 0;i<pL-1;i++)
//    {
//        for(unsigned int k = ni ; k<ni+9 ; k++)
//        {
//            tmp+=P[k];
//        }
//        std::stringstream ss(tmp);
//        ss >> p[p.size()-2-i];
//        tmp="";
//        ni=ni+9;
//    }
//    for(unsigned int k = ni ; k<P.size() ; k++)
//    {
//        tmp+=P[k];
//    }
//    std::stringstream ss(tmp);
//    ss >> p[p.size()-2-i];
//    if(tmp.size() == 0) p.erase(p.begin());
//    return p;
//}

//vector<long long> RightAssio(vector<long long>tar)
//{
//    string P="";
//    for(int i = tar.size()-2 ; i >= 0 ; i--)
//    {
//        P+= to_string(tar[i]);
//    }
//    return ConvToArray(P);
//}

bool isGreaterBig(vector<long long> fir,vector<long long>sec)
{
    if(fir.size() > sec.size()) return true;
    else if (fir.size() == sec.size())
    {
        for(int i = fir.size()-2 ; i >= 0 ; i--)
        {
            if(fir[i] > sec[i])
            {
                return true;
            }
            else if(fir[i] < sec[i])
            {
                break;
            }
        }
    }
    return false;
}
//bool isGreaterBigLeft(vector<long long> fir,vector<long long>sec)
//{
//    if(fir.size() > sec.size()) return true;
//    else if (fir.size() == sec.size())
//    {
//        if(GetNumberOfDigits(fir[0]) > GetNumberOfDigits(sec[0])) return true;
//        for(int i = fir.size()-2 ; i >= 0 ; i--)
//        {
//            if(fir[i] > sec[i])
//            {
//                return true;
//            }
//            else if(fir[i] < sec[i])
//            {
//                break;
//            }
//        }
//    }
//    return false;
//}

vector<long long> sum(vector<long long> fir,vector<long long>sec)
{
    int l;
    if(fir.size() > sec.size())
    {
        l = fir.size();
        sec.resize(l);
    }
    else
    {
        l = sec.size();
        fir.resize(l);
    }
    vector<long long>res;
    res.resize(l);
    for(int i = 0 ; i < l ; i++)
    {
        long long r;
        r=fir[i]+sec[i];
        if(r+res[i]>999999999)
        {
            res[i]+=r-1000000000;
            res[i+1]++;
        }
        else
        {
            res[i] +=r;
        }
    }
    for(int i = l-2 ; i > 0 ; i--)
    {
        if(res[i] == 0)
        {
            res.erase(res.end()-1);
        }
        else break;
    }
    return res;
}
vector<long long> sub(vector<long long> fir,vector<long long>sec)
{
    int l;
    vector<long long>temp;
    vector<long long>res;
    if(fir[fir.size()-1] == -1 && sec[sec.size()-1] == -1)
    {
        fir[fir.size()-1] = 0;
        sec[sec.size()-1] = 0;
        return sub(sec,fir);
    }
    else if(sec[sec.size()-1] == -1)
    {
        sec[sec.size()-1]=0;
        return sum(fir,sec);
    }
    else if(fir[fir.size()-1] == -1)
    {
        fir[fir.size()-1] = 0;
        res = sum(fir,sec);
        res[res.size()-1] = -1;
        return res;
    }
    if(isGreaterBig(fir,sec))
    {
        l = fir.size();
        res.resize(l);
        sec.resize(l);
    }
    else if(isGreaterBig(sec,fir))
    {
        l = sec.size();
        res.resize(l);
        fir.resize(l);
        temp = sec;
        sec = fir;
        fir = temp;
        res[res.size()-1] = -1;
    }
    else
    {
        res.resize(2);
        return res;
    }
    for(int i = 0 ; i < l-1 ; i++)
    {
        long long r;
        r=fir[i]-sec[i];
        if(r-res[i]<0)
        {
            res[i]+=r+1000000000;
            fir[i+1]--;
        }
        else
        {
            res[i] +=r;
        }
    }
    for(int i = l-2 ; i > 0 ; i--)
    {
        if(res[i] == 0)
        {
            res.erase(res.end()-1);
        }
        else break;
    }
    return res;
}
vector<long long> mul(vector<long long> fir,vector<long long>sec)
{
    vector<long long>res;
    res.resize(fir.size()+sec.size()-1);
    int l = 0;
    unsigned k;
    long long tempo,tempo1,carry=0,r5ama;
    for(unsigned i = 0 ; i < sec.size()-1; i++)
    {
        for(k = 0 ; k < fir.size()-1; k++)
        {
            tempo = sec[i] * fir[k];
            tempo+=carry;
            tempo1 = tempo %1000000000;
            res[k+l]+= tempo1;
            tempo1 = tempo - tempo1;
            carry= (tempo1)/1000000000;
            if(/*k==0 || i==fir.size()-2*/true)
            {
                if(res[k+l]> 999999999)
                {
                    r5ama = res[k+l];
                    res[k+l] = res[k+l]%1000000000;
                    res[k+l+1] += (r5ama - res[k+l])/1000000000;
                }
            }

        }
        res[k+l]+=carry;
        carry=0;
        l++;
    }
    l= res.size();
    for(int i = l-2 ; i > 0 ; i--)
    {
        if(res[i] == 0)
        {
            res.erase(res.end()-1);
        }
        else break;
    }
    if((fir[fir.size() - 1] == -1 && sec[sec.size() - 1]==0) || (fir[fir.size() - 1] == 0 && sec[sec.size() - 1]==-1)) res[res.size()-1] = -1;
    return res;
}
void TstMore(long long & tmp2,long long & res,vector<long long> & m1,vector<long long> & tmp,vector<long long> sec,vector<long long> las,int trial)
{
    tmp2=res-trial;
    m1[0] = tmp2;
    tmp = mul(sec,m1);
    while(isGreaterBig(tmp,las))
    {
        tmp2-=trial;
        m1[0] = tmp2;
        tmp = mul(sec,m1);
    }
    tmp2+=trial;
    res=tmp2;
}

long long TstFn(vector<long long>las,vector<long long>sec,vector<long long>m1)
{
    long long tmp1=m1[0],res=m1[0],tmp2;
    vector<long long> tmp;
    tmp= mul(sec,m1);
    if(isGreaterBig(tmp,las))
    {
        tmp2 = tmp1 - (tmp1/2);
        if (tmp1==tmp2) return res;
        m1[0] = tmp2;
        tmp = mul(sec,m1);
        if(isGreaterBig(tmp,las))
        {
            res = TstFn(las,sec,m1);
        }
        else
        {
            tmp2= tmp1 - (tmp1/4);
            if (tmp1==tmp2) return res;
            m1[0] = tmp2;
            tmp = mul(sec,m1);
            if(isGreaterBig(tmp,las))
            {
                res = TstFn(las,sec,m1);
            }
            else
            {
                tmp2= tmp1 - (tmp1/8);
                if (tmp1==tmp2) return res;
                m1[0] = tmp2;
                tmp = mul(sec,m1);
                if(isGreaterBig(tmp,las))
                {
                    res = TstFn(las,sec,m1);
                }
                else
                {
                    tmp2= tmp1 - (tmp1/16);
                    if (tmp1==tmp2) return res;
                    m1[0] = tmp2;
                    tmp = mul(sec,m1);
                    if(isGreaterBig(tmp,las))
                    {
                        res = TstFn(las,sec,m1);
                    }
                    else
                    {
                        tmp2= tmp1 - (tmp1/32);
                        if (tmp1==tmp2) return res;
                        m1[0] = tmp2;
                        tmp = mul(sec,m1);
                        if(isGreaterBig(tmp,las))
                        {
                            res = TstFn(las,sec,m1);
                        }
                        else
                        {
                            tmp2= tmp1 - (tmp1/64);
                            if (tmp1==tmp2) return res;
                            m1[0] = tmp2;
                            tmp = mul(sec,m1);
                            if(isGreaterBig(tmp,las))
                            {
                                res = TstFn(las,sec,m1);
                            }
                            else
                            {
                                TstMore(tmp2,res,m1,tmp,sec,las,1000);
                                TstMore(tmp2,res,m1,tmp,sec,las,500);
                                TstMore(tmp2,res,m1,tmp,sec,las,100);
                                TstMore(tmp2,res,m1,tmp,sec,las,50);
                                TstMore(tmp2,res,m1,tmp,sec,las,10);
                                TstMore(tmp2,res,m1,tmp,sec,las,5);
                                TstMore(tmp2,res,m1,tmp,sec,las,2);
                                return res;
                            }
                        }
                    }
                }
            }
        }
        return res;
    }
    else
    {
        return res;
    }
}

vector<long long> remBig(vector<long long>fir,vector<long long>sec)
{
    vector<long long>res,tmp;
    /*tmp = sub(fir,sec);
    if(isGreaterBig(sec,tmp))
        return tmp;
    else if(fir.size()==2 && sec.size()==2)
    {
        res.resize(2);
        res[0]=fir[0]/sec[0];
        tmp = sub(fir,mul(res,sec));
        return tmp;
    }*/
    res.resize(2);
    bool flag1;
    while(isGreaterBig(fir,sec))
    {
        if(fir[fir.size()-2] > sec [sec.size()-2])
        {
            res[0] = fir[fir.size()-2] / sec[sec.size()-2];
        }
        else
        {
            long long tst = (fir[fir.size()-2]*1000000000)+fir[fir.size()-3];
            res[0] = tst / sec[sec.size()-2];
        }
        tmp = mul(sec,res);
        flag1 = true;
        while(isGreaterBig(tmp,fir))
        {
            if(flag1)
            {
                res[0]=TstFn(fir,sec,res);
                tmp = mul(sec,res);
                flag1= 0;
            }
            else
            {
                res[0]--;
                tmp = sub(tmp,sec);
            }
        }
        fir = sub(fir,tmp);
    }

    return fir;
}

vector<long long> div3(vector<long long>fir,vector<long long>sec,int mode = 0)
{
    vector<long long>res,m1,tmp,las;
    tmp = sub(fir,sec);
    if(isGreaterBig(sec,fir))
    {
        if(mode==0)
        {
            res.resize(2);
            return res;
        }
        else
        {
            return fir;
        }
    }
    else if(isGreaterBig(sec,tmp))
    {
        if(mode==0)
        {
            res.resize(2);
            res[0]= 1;
            return res;
        }
        else
        {
            return tmp;
        }
    }
    else if(fir.size()==2 && sec.size()==2)
    {
        res.resize(2);
        res[0]=fir[0]/sec[0];
        if(mode==0)
        {
            return res;
        }
        else
        {
            tmp = sub(fir,mul(res,sec));
            return tmp;
        }
    }
    else
    {
        string FirStr="",LasStr="",Ba2iFir="",ya="",ya1="";
        int tmpCount = 0,i,Ba2iLen=0;
        res.resize(fir.size()-sec.size()+10);
        i = res.size()-2;
        m1.resize(2);
        while(isGreaterBig(fir,sec))
        {
            if(fir[fir.size()-2] > sec [sec.size()-2])
            {
                res[i] = fir[fir.size()-2] / sec[sec.size()-2];
            }
            else
            {
                long long tst = (fir[fir.size()-2]*1000000000)+fir[fir.size()-3];
                res[i] = tst / sec[sec.size()-2];
            }
            m1[0] = res[i];
            tmp = mul(sec,m1);
            tmpCount  = (9*(tmp.size()-2)) + GetNumberOfDigits(tmp[tmp.size()-2]);
            FirStr = ConvToString(fir);
            LasStr = FirStr.substr(0,tmpCount);
            if(int(FirStr.length()) >= tmpCount)
            {
                Ba2iLen = FirStr.length()-tmpCount;
                Ba2iFir = FirStr.substr(tmpCount,Ba2iLen);
            }
            else
            {
                Ba2iFir = "";
            }
            las = ConvToArray(LasStr);
            ya1 = to_string(las[las.size()-2]);
            ya = to_string(tmp[tmp.size()-2]);
            if((ya1[0] == '1' && ya1[1] == '0' && ya[0] == '9'))
            {
wrong:;         LasStr = FirStr.substr(0,tmpCount+1);
                Ba2iLen = FirStr.length()-tmpCount-1;
                Ba2iFir = FirStr.substr(tmpCount+1,Ba2iLen);
                las = ConvToArray(LasStr);
            }
            else if(ya[0] == '1' && ya1[0] == '9' && !isGreaterBig(tmp,las))
            {
                LasStr = FirStr.substr(0,tmpCount-1);
                Ba2iLen = FirStr.length()-tmpCount+1;
                Ba2iFir = FirStr.substr(tmpCount-1,Ba2iLen);
                las = ConvToArray(LasStr);
            }
            bool flag1 = true;
            while(isGreaterBig(tmp,las))
            {
                if(flag1)
                {
                    res[i]=TstFn(las,sec,m1);
                    m1[0] = res[i];
                    tmp = mul(sec,m1);
                    flag1= 0;
                }
                else
                {
                    if(res[i]==1) goto wrong;
                    res[i]--;
                    m1[0] = res[i];
                    tmp = sub(tmp,sec);
                }
            }
            las = sub(las,tmp);
            LasStr = ConvToString(las);
            FirStr = LasStr + Ba2iFir;
            fir = ConvToArray(FirStr);
            i--;
        }
        while(i>-1)
        {
            res.erase(res.begin());
            i--;
        }
        if (mode == 0) return res;
        else return fir;
    }
}

void div2(vector<long long>tar,vector<long long>&res)
{
    res.clear();
    int l = tar.size()-2;
    res.resize(l+2);
    long long carry = 0;
    for(int i = l ; i >= 0 ; i--)
    {
        res[i] = (tar[i]+carry)/2;
        if(tar[i]%2!=0)
        {
            carry = 1000000000;
        }
        else carry = 0;
    }
    l = res.size()-2;
    for(int i = l ; i > 0 ; i--)
    {
        if(res[i] == 0)
        {
            res.erase(res.end()-1);
        }
        else break;
    }
}
//vector<long long> div(vector<long long>fir,vector<long long>sec,int mode = 0)
//{
//    //if(GetNumberOfDigits(fir[fir.size()-2]) != 9) fir = LeftAssio(fir);
//    //if(GetNumberOfDigits(sec[sec.size()-2]) != 9) sec = LeftAssio(sec);
//    bool flag3 = true;
//    vector<long long>res,m1,tmp,las;
//    if(fir.size() < sec.size())
//    {
//        if(mode==0)
//        {
//            res.resize(2);
//            return res;
//        }
//        else
//        {
//            return fir;
//        }
//    }
//    else res.resize(fir.size()-sec.size()+10);
//    m1.resize(2);
//    long long r5ama;
//    int i = res.size()-2 , tmpCount = 0;
//    string tmpStr = "",ya;
//    while(isGreaterBig(fir,sec))
//    {
//        if(fir[fir.size()-2] >= sec [sec.size()-2])
//        {
//            if(i>=0) res[i] = fir[fir.size()-2] / sec[sec.size()-2];
//            else res.push_back(fir[fir.size()-2] / sec[sec.size()-2]);
//        }
//        else
//        {
//            long long tst = (fir[fir.size()-2]*1000000000)+fir[fir.size()-3];
//            if(i>=0) res[i] = tst / sec[sec.size()-2];
//            else res.push_back(res[i] = tst / sec[sec.size()-2]);
//        }
//        m1[0] = res[i];
//        tmp = mul(sec,m1);
//        ya = to_string(fir[fir.size()-2]);
//        if(ya[0] == '9' && !(sec.size() == 2 && GetNumberOfDigits(sec[0])==1))
//        {
//            tmpCount  = (9*(tmp.size()-2)) + GetNumberOfDigits(tmp[tmp.size()-2]);
//            tmpStr = ConvToString(fir);
//            las = ConvToArray(tmpStr.substr(0,tmpCount-1));
//        }
//        else las = vector<long long>(fir.end()-tmp.size(),fir.end());
//        bool flag1 = 1;
//        while(isGreaterBig(tmp,las))
//        {
//            if(flag1)
//            {
//                res[i]=TstFn(las,sec,m1);
//                m1[0] = res[i];
//                tmp = mul(sec,m1);
//                flag1= 0;
//            }
//            else
//            {
//                res[i]--;
//                m1[0] = res[i];
//                tmp = sub(tmp,sec);
//                //if(res[i]==0) goto araf;
//            }
//        }
//        int l = fir.size() - tmp.size();
//        for(int k = 0 ; k < l ; k++)
//        {
//            tmp.insert(tmp.begin(),0);
//            tmp[0]=0;
//        }
//        fir = sub(fir,tmp);
//        if(res[i]>999999999)
//        {
//            r5ama = res[i];
//            res[i-1] = res[i]%1000000000;
//            res[i] = (r5ama - res[i-1])/1000000000;
//            i--;
//        }
//        flag3=false;
//        i--;
//    }
//    while(i!=-1)
//    {
//        res.erase(res.begin());
//        i--;
//    }
//    for(int i = res.size()-2 ; i > 0 ; i--)
//    {
//        if(res[i] == 0)
//        {
//            res.erase(res.end()-1);
//        }
//        else break;
//    }
//    if (mode == 0) return res;
//    else return fir;
//}
vector<long long>ChooseBest(vector<long long>&fir,vector<vector<long long>>choices)
{
    vector<long long>tmp,res,TmpFir,t;
    int num,index=-1,Ba2i=0,k;
    string Fir="",Ba2iFir="",Res="";
    bool flag=true,flag2=false;
    res.resize(0);
    Fir = ConvToString(fir);
    re:;
    for(k = 0 ; k < 4 ; k++)
    {
        tmp = choices[k];
        num = GetNumberOfDigits(tmp[tmp.size()-2]) + (9*(tmp.size()-2));
        if(flag2)
            num++;
        TmpFir = ConvToArray(Fir.substr(0,num));
        if((isGreaterBig(TmpFir,tmp) || IsEqual(TmpFir,tmp)) && flag)
        {
            t = sub(TmpFir,tmp);
            res=choices[k];
            index = k;
            flag = false;
        }
        else if(isGreaterBig(t,sub(TmpFir,tmp)) && (isGreaterBig(TmpFir,tmp)||IsEqual(TmpFir,tmp)))
        {
            res=choices[k];
            index = k;
            t = sub(TmpFir,tmp);
        }
    }
    if(res.size()==0)
    {
        flag2=true;
        goto re;
    }
    num = GetNumberOfDigits(res[res.size()-2]) + (9*(res.size()-2));
    if(flag2)
        num++;
    if(int(Fir.length()) < num) num=Fir.length();
    Ba2i = Fir.length() - num;
    Ba2iFir = Fir.substr(num,Ba2i);
    Fir = ConvToString(t) + Ba2iFir;
    fir = ConvToArray(Fir);
    if(index == 0) index = 1;
    else if(index == 1) index = 2;
    else if(index == 2) index = 4;
    else if(index == 3) index = 8;
    Res = to_string(index);
    for(int j = 0 ; j < Ba2i ; j++) Res+="0";
    res = ConvToArray(Res);
    return res;
}

vector<long long>divM(vector<long long>fir,vector<long long>sec,int mode=0)
{
    vector<long long>two,res;
    if(sec.size() == 2)
    {
        if(fir.size()==2)
        {
            res.resize(2);
            if(mode==0)
                res[0]=fir[0]/sec[0];
            else
                res[0]=fir[0]%sec[0];
            return res;
        }
        if(sec[0]==2)
        {
            div2(fir,res);
            if(mode==0) return res;
            else
            {
                res.clear();
                res.resize(2);
                if(fir[0]%2!=0)
                {
                    res[0]=1;
                }
                return res;
            }
        }
    }
    vector<vector<long long>>choices;
    res.resize(2);
    two.resize(2);
    two[0]=2;
    choices.resize(4);
    choices[0]=sec;
    choices[1]=mul(sec,two);
    choices[2]=mul(choices[1],two);
    choices[3]=mul(choices[2],two);
    while(isGreaterBig(fir,sec) || IsEqual(fir,sec))
    {
        res = sum(ChooseBest(fir,choices),res);
    }
    if(mode==0)
        return res;
    else
        return fir;

}
//vector<long long>Redu(vector<long long>tmp,vector<long long>mod)
//{
//    vector<long long>use,res;
//    use = mod;
//    mod.insert(mod.begin(),0);
//    if(isGreaterBig(tmp,mod))
//    {
//       res = Redu(tmp,mod);
//    }
//    else
//    {
//        string ve,ve1;
//        ve1 = ConvToString(use);
//        ve = ve1+"0000";
//        mod = ConvToArray(ve);
//        if(isGreaterBig(tmp,mod))
//        {
//            res = Redu(tmp,mod);
//        }
//        else
//        {
//            ve = ve1+"00";
//            mod = ConvToArray(ve);
//            if(isGreaterBig(tmp,mod))
//            {
//                res = Redu(tmp,mod);
//            }
//            else
//            {
//                ve = ve1+"0";
//                mod = ConvToArray(ve);
//                if(isGreaterBig(tmp,mod))
//                {
//                    res = Redu(tmp,mod);
//                }
//                else
//                {
//                    mod.clear();
//                    mod.resize(2);
//                    mod[0]=5;
//                    mod = mul(use,mod);
//                    if(isGreaterBig(tmp,mod))
//                    {
//                        res = Redu(tmp,mod);
//                    }
//                    else
//                    {
//                        return use;
//                    }
//                }
//            }
//        }

//    }
//    return res;
//}

vector<long long> rem2(vector<long long>fir, vector<long long> mod)
{
    if(isGreaterBig(mod,fir))
    {
        return fir;
    }
    else if(IsEqual(fir,mod))
    {
        vector<long long>r;
        r.resize(0);
        return r;
    }
    vector<long long> remainder,tmpo1,tmp,res,tmp1;
    long long tst=0;
    bool flag1;
    remainder.resize(1);
    tmpo1.resize(2);
    res.resize(2);
    for (int i = fir.size()-2; i >= 0; i--)
    {
        remainder.insert(remainder.begin(),0);
        tmpo1[0] = fir[i];
        tmp = sum(remainder,tmpo1);
        if(isGreaterBig(tmp,mod)||IsEqual(tmp,mod))
        {
            remainder = divM(tmp, mod,1);

//            while(isGreaterBig(tmp,mod))
//            {
//                res[0]=999999999;
//                tmp1 = mul(mod,res);
//                flag1 = true;
//                while(isGreaterBig(tmp1,tmp))
//                {
//                    if(flag1)
//                    {
//                        res[0]=TstFn(tmp,mod,res);
//                        tmp1 = mul(mod,res);
//                        flag1= 0;
//                    }
//                    else
//                    {
//                        res[0]--;
//                        tmp1 = sub(tmp1,mod);
//                    }
//                }
//                tmp = sub(tmp,tmp1);
//            }
        }
        else remainder = tmp;
    }
    return remainder;
}
vector<long long> rem1(vector<long long>fir, vector<long long> mod)
{
    if(isGreaterBig(mod,fir))
    {
        return fir;
    }
    else if(IsEqual(fir,mod))
    {
        vector<long long>r;
        r.resize(0);
        return r;
    }
    string P = ConvToString(fir);
    vector<long long> remainder,tmpo1,tmp,ten;
    remainder.resize(1);
    ten.resize(2);
    ten[0]=10;
    tmpo1.resize(2);
    for (int i = 0; P[i] != '\0'; ++i)
    {
        remainder = mul(remainder,ten);
        tmpo1[0] = P[i]-'0';
        tmp = sum(remainder,tmpo1);
        while(isGreaterBig(tmp,mod)||IsEqual(tmp,mod))
        {
            tmp=sub(tmp,mod);
        }
        remainder = tmp;
    }
    return remainder;
}

vector<long long> rem(vector<long long>fir, vector<long long> mod)
{
    if(isGreaterBig(mod,fir))
    {
        return fir;
    }
    else if(IsEqual(fir,mod))
    {
        vector<long long>r;
        r.resize(2);
        return r;
    }
    string P = ConvToString(fir),R="";
    vector<long long>remainder,use,ten;
    remainder.resize(2);
    use.resize(2);
    ten.resize(2);
    ten[0]=10;
    for (int i = 0; P[i] != '\0'; ++i)
    {
        remainder = mul(remainder,ten);
        use[0] = P[i]-'0';
        remainder = sum(remainder,use);
        while(isGreaterBig(remainder,mod) || IsEqual(remainder,mod))
            remainder = sub(remainder,mod);
    }
    return remainder;
}

void powe(vector<long long>fir,vector<long long>sec,vector<long long> mod, vector<long long>&res)
{
    if(sec[0]==2)
    {
        res = mul(fir,fir);
        res = rem(res,mod);
        return;
    }
    if(sec[0]%2 != 0)
    {
        sec[0]--;
        powe(fir,sec,mod,res);
        res = mul(res,fir);
//        if(file == 1)
//        {
//            myfile << "res: " << ConvToString(res) << endl;
//            myfile << "mod: " << ConvToString(mod) << endl;
//        }
        res = rem(res,mod);
//        if(file == 1)
//        myfile << ConvToString(res) << endl;
    }
    else
    {
        div2(sec,sec);
        powe(fir,sec,mod,res);
        res = mul(res,res);
//        if(file == 1)
//        {
//            myfile << "res: " << ConvToString(res) << endl;
//            myfile << "mod: " << ConvToString(mod) << endl;
//        }
        res = rem(res,mod);
//        if(file == 1)
//        myfile << ConvToString(res) << endl;
    }
    return;
}

bool IsEqualZero(vector<long long>n)
{
    if(n.size() == 2)
    {
        if(n[0]==0) return true;
        else return false;
    }
    else
    {
        return false;
    }
}
bool IsEqualOne(vector<long long>n)
{
    if(n.size() == 2)
    {
        if(n[0]==1) return true;
        else return false;
    }
    else
    {
        return false;
    }
}


bool isPrime(vector<long long> n)
{
    if(n[0]%2 == 0) return false;
    if(n.size()==2)
    {
        if(n[0]==2 || n[0]==3) return true;
        else if(n[0]==1 || n[0]==4) return false;
    }
    vector<long long>q,res,a,tmp,tmp2;
    q=n;
    q[0]--;
    a.resize(2);
    tmp2.resize(2);
    long long k=0;
    while(q[0]%2==0)
    {
        div2(q,q);
        k++;
    }
    a[0]=2;
    powe(a,q,n,res);
    if(res.size()==2)
    {
        if(res[0]==1) return true;
    }
    tmp=n;
    tmp[0]--;
    if(IsEqual(res,tmp)) return true;
    for(int i = 1 ; i <= k-1;i++)
    {
        res = divM(mul(res,res),n,1);
        if(IsEqual(res,tmp)) return true;
    }
    return false;
}

vector<long long>ExtEc(vector<long long>tar,vector<long long>mod)
{
    vector<long long>A2,A3,B2,B3,Q,T2,T3;
    A2.resize(2);
    A3 = mod;
    B2.resize(2);
    B2[0]=1;
    B3 = tar;
    while(!IsEqualZero(B3) && !IsEqualOne(B3))
    {
        Q = divM(A3,B3);
        T2 = mul(Q,B2);
        T2 = sub(A2,T2);
        T3 = mul(Q,B3);
        T3 = sub(A3,T3);
        A2 = B2;
        A3 = B3;
        B2 = T2;
        B3 = T3;
    }
    cout << ConvToString(B2);
    if(IsEqualOne(B3))
    {

        while(B2[B2.size()-1]==-1)
        {
            B2[B2.size()-1] = 0;
            B2 = sub(mod,B2);
        }
        return B2;
    }
    else
    {
        vector<long long>no;
        no.resize(2);
        no[0]=-1;
        return no;
    }
}



int main()
{
    string P,Q,E,M;
    //cin >> P >> Q >> E;
    bool flag = false,flag1=false;
    P ="P=12369571528747655798110188786567180759626910465726920556567298659370399748072366507234899432827475865189642714067836207300153035059472237275816384410077871";
    Q ="Q=2065420353441994803054315079370635087865508423962173447811880044936318158815802774220405304957787464676771309034463560633713497474362222775683960029689473";
    E ="E=65537";
    M ="M=1976620216402300889624482718775150";
    if (P[0] == 'P')
        P = P.substr(2,P.size()-1);
    if (Q[0] == 'Q')
        Q = Q.substr(2,Q.size()-1);
    if (E[0] == 'E')
        E = E.substr(2,E.size()-1);
    if (M[0] == 'M')
        M = M.substr(2,M.size()-1);
    vector<long long>q,p,e,m,n,d,PhiN,one,c;
    p = ConvToArray(P);
    q = ConvToArray(Q);
    e = ConvToArray(E);
    m = ConvToArray(M);
//    for(int i=0;i<1000;i++)
//    {
////        r=mul(p,q);
//        d = rem2(p,q);
//    }
    flag = isPrime(q);
    flag1 = isPrime(q);
    if(flag) cout << "yes\n";
    if(flag1) cout << "yes\n";
    one.resize(2);
    one[0]=1;
    n = mul(p,q);
    PhiN = mul(sub(p,one),sub(q,one));
    d = ExtEc(e,PhiN);
    powe(m,e,n,c);
    m.clear();
    string x = ConvToString(c);
    cout << x << endl;
    cout << ConvToString(d) << endl;
    cout << ConvToString(n) << endl;
    powe(c,d,n,m);
    cout << ConvToString(m) << endl;
   //    BigNumber * p1 = new BigNumber(P);
   //    BigNumber * q1= new BigNumber(Q);
   //    BigNumber * e1=new BigNumber(E);
   //    BigNumber *r1= new BigNumber();
    return 0;
}
