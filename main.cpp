#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>

using namespace std;

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
    return p;
}


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

long long TstFn(vector<long long>las,vector<long long>sec,vector<long long>m1)
{
    long long tmp1=m1[0],res=m1[0],tmp2;
    vector<long long> tmp = mul(sec,m1);
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
vector<long long> div(vector<long long>fir,vector<long long>sec,int mode = 0)
{
    vector<long long>res,m1,tmp;
    res.resize(fir.size()-sec.size()+2);
    m1.resize(2);
    unsigned i = res.size()-2;
    while(isGreaterBig(fir,sec))
    {
        //if(isGreaterBig(sec,fir)) break;
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
        vector<long long>las(fir.end()-tmp.size(),fir.end());
        bool flag1 = 1;
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
                res[i]--;
                m1[0] = res[i];
                tmp = sub(tmp,sec);
            }
        }
        unsigned l = fir.size() - tmp.size();
        for(unsigned k = 0 ; k < l ; k++)
        {
            tmp.insert(tmp.begin(),0);
            tmp[0]=0;
        }
        fir = sub(fir,tmp);
        i--;
    }
    while(i!=-1)
    {
        res.erase(res.begin());
        i--;
    }
    if (mode == 0) return res;
    else return fir;
}

int main()
{
    string P,Q,E;
    vector<long long>q,p,e;
    //cin >> P >> Q >> E;

    P ="P=12369571528747655798110188786567180759626910465726920556567298659370399748072366507234899432827475865189642714067836207300153035059472237275816384410077871";
    Q ="Q=20654203534419948030543150793706350878655084239621734478118800449363181588158027742204053049577874646767713090";
    E ="E=65537";
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
    //r = sum(p,q);
    //r = sub(q,p);
    //for(int i=0;i<1000;i++)
    //r = mul(p,q);
    r=div(p,q);
    cout << endl;
    for(int i = r.size()-1 ; i >= 0 ; i--)
            cout << r[i];
    cout <<"\nend";
    return 0;
}
