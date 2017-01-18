#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

string rev(string s)
{
    string re="";
    for(unsigned int i = 0 ; i < s.size() ; i++)
    {
        re+= s[s.size() - 1 -i];
    }
    return re;
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
        std::stringstream ss(rev(tmp));
        ss >> p[i];
        tmp="";
        ni=ni+9;
    }
    for(unsigned int k = ni ; k<P.size() ; k++)
    {
        tmp+=P[P.size() - 1 - k];
    }
    std::stringstream ss(rev(tmp));
    ss >> p[p.size()-2];
    return p;
}

vector<long long> sum(vector<long long> fir,vector<long long>sec)
{
    int l;
    if(fir.size() > sec.size()) l = fir.size();
    else l = sec.size();
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
    return res;
}
vector<long long> sub(vector<long long> fir,vector<long long>sec)
{
    int l;
    vector<long long>temp;
    vector<long long>res;
    if(fir.size() > sec.size())
    {
        l = fir.size();
        res.resize(l);
    }
    else if (fir.size() == sec.size())
    {
        l = fir.size();
        res.resize(l);
        if(fir[fir.size()-2] < sec[sec.size()-2])
        {
            temp = sec;
            sec = fir;
            fir = temp;
            res[res.size()-1] = -1;
        }
    }
    else
    {
        l = sec.size();
        res.resize(l);
        temp = sec;
        sec = fir;
        fir = temp;
        res[res.size()-1] = -1;
    }
    for(int i = 0 ; i < l ; i++)
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
    return res;
}
vector<long long> mul(vector<long long> fir,vector<long long>sec)
{
    vector<long long>res;
    res.resize(fir.size()+sec.size()-2);
    vector<vector<long long>> cal;
    vector<long long>temp;
    temp.resize(fir.size()+sec.size());
    cal.resize(sec.size());
    int l = 0,k;
    long long tempo,tempo1,carry=0,r5ama;
    for(int i = 0 ; i < sec.size()-1; i++)
    {
        for(k = 0 ; k < fir.size()-1; k++)
        {
            tempo = sec[i] * fir[k];
            tempo+=carry;
            tempo1 = tempo %1000000000;
            res[k+l]+= tempo1;
            tempo1 = tempo - tempo1;
            carry= (tempo1)/1000000000;
            if(k==0 || i==fir.size()-2)
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
    if((fir[fir.size() - 1] == -1 && sec[sec.size() - 1]==0) || (fir[fir.size() - 1] == 0 && sec[sec.size() - 1]==-1)) res[res.size()-1] = -1;
    return res;
}

int main()
{
    string P,Q,E;
    vector<long long>q,p,e;
    cin >> P >> Q >> E;
    if (P[0] == 'P')
        P = P.substr(2,P.size()-1);
    if (Q[0] == 'Q')
        Q = Q.substr(2,Q.size()-1);
    if (E[0] == 'E')
        E = E.substr(2,E.size()-1);
    p = ConvToArray(P);
    q = ConvToArray(Q);
    e = ConvToArray(E);
    vector<long long> r  ;
    r = sum(p,q);
    r = sub(q,p);
    r = mul(p,q);
    return 0;
}
