"""PAK-LIAR audit: provenance, cleaning, and H1-H5 analyses (seed 42).
Usage: python pakliar_audit_analysis.py pak-Liar.xlsx
Outputs: results.json (all statistics) and pakliar_clean.csv (analytic sample, N = 4,714).
Requires: Python 3.12, pandas, numpy, scipy, scikit-learn (tested with 1.8).
"""
import re, json, warnings, numpy as np, pandas as pd
from scipy import stats
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression, LogisticRegressionCV
from sklearn.model_selection import StratifiedKFold, StratifiedGroupKFold
from sklearn.metrics import roc_auc_score, accuracy_score, f1_score
from sklearn.preprocessing import StandardScaler
from scipy.sparse import hstack, csr_matrix
warnings.filterwarnings("ignore")
SEED=42; rng=np.random.default_rng(SEED)
R={}

# ---------- load ----------
import sys
raw=pd.read_excel(sys.argv[1] if len(sys.argv)>1 else 'pak-Liar.xlsx')  # path to the Kaggle release
raw.columns=['id','label','statement','subject','speaker','job','state','party','venue']
raw['y']=raw.label.astype(int)            # TRUE=1
raw['corpus_id']=np.where(raw.id>=4558,'PAK','US')
s=raw.state.fillna('').str.strip().str.lower(); p=raw.party.fillna('').str.strip().str.lower()
pakst={'punjab','sindh','kpk','balochistan','federal','gb','gilgit-baltistan','azad kashmir'}
pakp=['pti','pml-n','pmln','ppp','pppp','mqm','anp','jui-f','pml-q']
raw['corpus_rule']=np.where(s.isin(pakst)|p.apply(lambda x:any(t in x for t in pakp)),'PAK','US')
ct=pd.crosstab(raw.corpus_id,raw.corpus_rule)
R['agreement']={'table':ct.to_dict(),'kappa':None}
po=(raw.corpus_id==raw.corpus_rule).mean()
pe=sum((raw.corpus_id==k).mean()*(raw.corpus_rule==k).mean() for k in ['US','PAK'])
R['agreement'].update(accuracy=po,kappa=(po-pe)/(1-pe),
    rule_pak_precision=((raw.corpus_rule=='PAK')&(raw.corpus_id=='PAK')).sum()/(raw.corpus_rule=='PAK').sum(),
    rule_pak_recall=((raw.corpus_rule=='PAK')&(raw.corpus_id=='PAK')).sum()/(raw.corpus_id=='PAK').sum())
R['raw']={'n':len(raw),'by_corpus':pd.crosstab(raw.corpus_id,raw.y).to_dict()}

# ---------- cleaning ----------
raw['norm']=raw.statement.str.strip().str.strip('"').str.strip().str.lower()
test_row=raw.id==1913
dup_norm=raw.sort_values('id').duplicated('norm',keep='first')
dup_norm=dup_norm.reindex(raw.index)
R['clean_steps']={'test_row':int(test_row.sum()),
  'dup_exact':int(raw.statement.duplicated().sum()),
  'dup_normalised':int(dup_norm.sum()),
  'dup_by_corpus':raw[dup_norm].corpus_id.value_counts().to_dict(),
  'conflicting_label_groups':int((raw.groupby('norm').y.nunique()>1).sum())}
raw['group']=raw.groupby('norm').ngroup()
full=raw[~test_row].copy()                  # duplicates kept (for sensitivity)
d=raw[~test_row & ~dup_norm].copy()         # primary analytic sample
R['clean']={'n':len(d),'by_corpus':pd.crosstab(d.corpus_id,d.y).to_dict()}

# ---------- features ----------
QUANT=["all","always","any","anybody","anyone","anything","anywhere","both","completely",
"constantly","each","either","entire","entirely","every","everybody","everyone","everything",
"everywhere","ever","forever","fully","never","neither","no","nobody","none","nothing",
"nowhere","only","totally","absolutely","whole","whatsoever","universally"]
assert len(QUANT)==35
STOP=set("""a an the this that these those he she it we they i you his her its our their my your
in on at of for to from by with and or but if as is are was were be been not no so than then
there here what which who whom whose when where why how all any each every some such mr mrs ms dr
says said say""".split())
def syll(w):
    w=w.lower(); v=re.findall(r'[aeiouy]+',w); n=len(v)
    if w.endswith('e') and n>1 and not w.endswith(('le','ee')): n-=1
    return max(n,1)
def feats(t):
    t=str(t).strip().strip('"').strip()
    toks=re.findall(r"[A-Za-z]+(?:'[A-Za-z]+)?",t)
    words=[w.lower() for w in toks]; n=len(words)
    sents=max(len([x for x in re.split(r'[.!?]+',t) if x.strip()]),1)
    qd=100*sum(w in QUANT for w in words)/n if n else 0
    ttr=len(set(words))/n if n else 0
    fk=0.39*(n/sents)+11.8*(sum(syll(w) for w in words)/n)-15.59 if n else 0
    # NE proxy: capitalised tokens not at sentence start, excluding stop/function words
    ne=0; start=True
    for m in re.finditer(r"[A-Za-z][A-Za-z']*|[.!?]",t):
        tok=m.group()
        if tok in '.!?': start=True; continue
        if not start and tok[0].isupper() and tok.lower() not in STOP: ne+=1
        start=False
    return pd.Series({'n_words':n,'quant_dens':qd,'ttr':ttr,'fk_grade':fk,'ne_count':ne})
TF=['n_words','quant_dens','ttr','fk_grade','ne_count']
for df in (d,full): df[TF]=df.statement.apply(feats)

def venue_tier(v):
    v=str(v).lower() if pd.notna(v) else ''
    if not v.strip(): return 'Unknown'
    if re.search(r'fake|rumou?r|clickbait|satire|hoax|meme|propaganda|defamat',v): return 'Suspect'
    if re.search(r'tweet|twitter|facebook|blog|youtube|whatsapp|tiktok|instagram|e-?mail|social media|post on|online post|website|web site|newsletter|reddit|forum',v): return 'Unmediated UGC'
    if re.search(r'\btv ad|television ad|radio|campaign ad|\bad\b|advertisement|commercial|rally|mailer|robocall|flier|flyer|campaign',v): return 'Partisan broadcast'
    if re.search(r'press release|speech|news conference|press conference|debate|interview|statement|hearing|floor|testimony|address|op-ed|editorial|article|report|briefing|letter|meeting|forum|town hall|remarks|news|announcement|notification|verdict|result|budget|official',v): return 'Institutional'
    return 'Unknown'
INACC=r'clickbait|bot|social media|anonymous|fake|scam|troll|opponent|unknown|none|n/a'
for df in (d,full):
    df['venue_tier']=df.venue.apply(venue_tier)
    j=df.job.fillna('').astype(str).str.strip()
    df['inacc']=((j=='')|j.str.lower().str.contains(INACC)).astype(int)

# ---------- descriptives ----------
def comp(df):
    out={}
    for c,g in df.groupby('corpus_id'):
        out[c]={k:round(100*(1-g[k].isna().mean()),1) for k in ['speaker','job','state','party','venue','subject']}
        out[c]['n_words_mean']=round(g.n_words.mean(),2)
    return out
R['completeness']=comp(d)
chi=stats.chi2_contingency(pd.crosstab(d.corpus_id,d.y))
n=len(d); R['corpus_label_chi']={'chi2':chi[0],'p':chi[1],'df':chi[2],'V':np.sqrt(chi[0]/n)}
R['venue_tab']=pd.crosstab(d.venue_tier,d.corpus_id).to_dict()
R['inacc_tab']=pd.crosstab(d.inacc,d.corpus_id).to_dict()
R['quoted_share']={c:float(g.statement.str.strip().str.startswith(('"','“')).mean()) for c,g in d.groupby('corpus_id')}

# ---------- H1 ----------
def bh(p):
    p=np.asarray(p,float); m=len(p); o=np.argsort(p); q=np.empty(m)
    q[o]=np.minimum.accumulate((p[o]*m/np.arange(1,m+1))[::-1])[::-1]; return np.minimum(q,1)
H1=[]
for blk,g in [('Overall',d),('US',d[d.corpus_id=='US']),('PAK',d[d.corpus_id=='PAK'])]:
    rows=[]
    for f in TF:
        a=g.loc[g.y==0,f]; b=g.loc[g.y==1,f]
        t,pv=stats.ttest_ind(a,b,equal_var=False)
        va,vb=a.var(ddof=1),b.var(ddof=1); na,nb=len(a),len(b)
        se=np.sqrt(va/na+vb/nb); dfw=(va/na+vb/nb)**2/((va/na)**2/(na-1)+(vb/nb)**2/(nb-1))
        diff=a.mean()-b.mean(); tc=stats.t.ppf(.975,dfw)
        sp=np.sqrt(((na-1)*va+(nb-1)*vb)/(na+nb-2))
        rows.append(dict(block=blk,feature=f,mF=a.mean(),sdF=a.std(),mT=b.mean(),sdT=b.std(),diff=diff,
            lo=diff-tc*se,hi=diff+tc*se,t=t,p=pv,d=diff/sp,nF=na,nT=nb))
    q=bh([r['p'] for r in rows])
    for r,qq in zip(rows,q): r['p_bh']=qq
    H1+=rows
R['H1']=H1

# ---------- logistic regression with SEs (IRLS) ----------
def logit(X,y,ridge=1e-8,it=100):
    X=np.asarray(X,float); y=np.asarray(y,float); b=np.zeros(X.shape[1])
    for _ in range(it):
        eta=np.clip(X@b,-30,30); mu=1/(1+np.exp(-eta)); W=mu*(1-mu)
        H=X.T@(X*W[:,None])+ridge*np.eye(X.shape[1]); g=X.T@(y-mu)
        step=np.linalg.solve(H,g); b+=step
        if np.max(np.abs(step))<1e-8: break
    eta=np.clip(X@b,-30,30); mu=1/(1+np.exp(-eta))
    H=X.T@(X*(mu*(1-mu))[:,None])+ridge*np.eye(X.shape[1])
    se=np.sqrt(np.diag(np.linalg.pinv(H)))
    ll=np.sum(y*np.log(np.clip(mu,1e-12,1))+(1-y)*np.log(np.clip(1-mu,1e-12,1)))
    return b,se,ll
VREF='Institutional'; VLV=['Partisan broadcast','Unmediated UGC','Suspect','Unknown']
def src_mat(df,levels=VLV):
    M=pd.DataFrame({f'v_{l}':(df.venue_tier==l).astype(int) for l in levels},index=df.index)
    M['inacc']=df.inacc; return M

# H2 descriptive FALSE rates + chi-square + ORs
def wilson(k,n,z=1.96):
    if n==0: return (np.nan,np.nan)
    ph=k/n; den=1+z*z/n; c=(ph+z*z/(2*n))/den; h=z*np.sqrt(ph*(1-ph)/n+z*z/(4*n*n))/den; return (c-h,c+h)
H2=[]
for blk,g in [('Overall',d),('US',d[d.corpus_id=='US']),('PAK',d[d.corpus_id=='PAK'])]:
    for col,cats in [('venue_tier',[VREF]+VLV),('inacc',[0,1])]:
        for c in cats:
            gg=g[g[col]==c]; k=int((gg.y==0).sum()); nn=len(gg)
            if nn==0: continue
            lo,hi=wilson(k,nn)
            H2.append(dict(block=blk,var=col,cat=str(c),n=nn,nF=k,rate=k/nn,lo=lo,hi=hi))
R['H2_rates']=H2
chis={}
for blk,g in [('Overall',d),('US',d[d.corpus_id=='US']),('PAK',d[d.corpus_id=='PAK'])]:
    for col in ['venue_tier','inacc']:
        tab=pd.crosstab(g[col],g.y); tab=tab[(tab.sum(1)>0)]
        c2=stats.chi2_contingency(tab); ex_min=c2[3].min()
        chis[f'{blk}_{col}']=dict(chi2=c2[0],df=c2[2],p=c2[1],V=np.sqrt(c2[0]/(len(g)*(min(tab.shape)-1))),min_expected=ex_min)
R['H2_chi']=chis
S=src_mat(d); X=np.column_stack([np.ones(len(d)),S.values]); b,se,_=logit(X,d.y)
R['H2_OR']=[dict(term=t,OR=np.exp(bb),lo=np.exp(bb-1.96*ss),hi=np.exp(bb+1.96*ss),p=2*stats.norm.sf(abs(bb/ss))) for t,bb,ss in zip(['const']+list(S.columns),b,se)][1:]

# ---------- H3 ----------
Z=(d[TF]-d[TF].mean())/d[TF].std()
pak=(d.corpus_id=='PAK').astype(int).values
Sx=src_mat(d); Sx=Sx.loc[:,Sx.sum()>0]
Xa=np.column_stack([np.ones(len(d)),pak,Z.values,Sx.values])
Xi=np.column_stack([Xa,Z.values*pak[:,None]])
_,_,lla=logit(Xa,d.y); bi,sei,lli=logit(Xi,d.y)
LR=2*(lli-lla); R['H3_LR']=dict(chi2=LR,df=len(TF),p=stats.chi2.sf(LR,len(TF)))
k0=Xa.shape[1]; inter=[]
for i,f in enumerate(TF):
    bb,ss=bi[k0+i],sei[k0+i]; inter.append(dict(feature=f,b=bb,se=ss,z=bb/ss,p=2*stats.norm.sf(abs(bb/ss))))
for r,q in zip(inter,bh([r['p'] for r in inter])): r['p_bh']=q
R['H3_inter']=inter
per={}
for c in ['US','PAK']:
    m=(d.corpus_id==c).values; Sc=Sx[m]; Sc=Sc.loc[:,(Sc.sum()>0)&(Sc.sum()<m.sum())]
    Xc=np.column_stack([np.ones(m.sum()),Z.values[m],Sc.values]); bc,sc,_=logit(Xc,d.y.values[m])
    per[c]={'coef':{f:(bc[1+i],sc[1+i],2*stats.norm.sf(abs(bc[1+i]/sc[1+i]))) for i,f in enumerate(TF)},
            'src':{col:(bc[1+len(TF)+j],sc[1+len(TF)+j]) for j,col in enumerate(Sc.columns)}}
R['H3_per']=per
wald=[]
for f in TF:
    (b1,s1,_),(b2,s2,_)=per['US']['coef'][f],per['PAK']['coef'][f]
    zz=(b1-b2)/np.sqrt(s1**2+s2**2); wald.append(dict(feature=f,diff=b1-b2,p=2*stats.norm.sf(abs(zz))))
for r,q in zip(wald,bh([r['p'] for r in wald])): r['p_bh']=q
R['H3_wald']=wald

# ---------- modelling helpers ----------
def tfidf(): return TfidfVectorizer(ngram_range=(1,2),min_df=3,max_features=3000,max_df=0.9,lowercase=True,token_pattern=r"(?u)\b[a-zA-Z][a-zA-Z]+\b")
def enet(cw=None): return LogisticRegressionCV(Cs=6,cv=StratifiedKFold(3,shuffle=True,random_state=SEED),penalty='elasticnet',solver='saga',l1_ratios=[0.9],scoring='roc_auc',max_iter=1000,tol=5e-3,class_weight=cw,random_state=SEED)
def build(model,tr,te):
    """returns fitted predict_proba on te"""
    ytr=tr.y.values
    if model=='M1':
        A=src_mat(tr); Bm=src_mat(te); keep=A.columns[A.sum()>0]
        clf=LogisticRegression(penalty=None,max_iter=2000); clf.fit(A[keep],ytr); return clf.predict_proba(Bm[keep])[:,1]
    sc=StandardScaler().fit(tr[TF]); Ttr=sc.transform(tr[TF]); Tte=sc.transform(te[TF])
    parts_tr=[Ttr]; parts_te=[Tte]
    if model in ('M3','M4'):
        A=src_mat(tr); Bm=src_mat(te); keep=A.columns[A.sum()>0]
        parts_tr.append(A[keep].values); parts_te.append(Bm[keep].values)
    if model in ('M4','TX'):
        v=tfidf().fit(tr.statement); parts_tr.insert(0,v.transform(tr.statement)); parts_te.insert(0,v.transform(te.statement))
    Xtr=hstack([csr_matrix(x) for x in parts_tr]).tocsr(); Xte=hstack([csr_matrix(x) for x in parts_te]).tocsr()
    clf=enet('balanced' if model in ('M4','TX') else None); clf.fit(Xtr,ytr); return clf.predict_proba(Xte)[:,1]
def cv_eval(df,models,grouped=False):
    out={m:[] for m in models}; oof={m:np.zeros(len(df)) for m in models}
    spl=(StratifiedGroupKFold(5,shuffle=True,random_state=SEED).split(df,df.y,df.group) if grouped
         else StratifiedKFold(5,shuffle=True,random_state=SEED).split(df,df.y))
    for tr,te in spl:
        a,b_=df.iloc[tr],df.iloc[te]
        for m in models:
            pr=build(m,a,b_); oof[m][te]=pr; out[m].append(roc_auc_score(b_.y,pr))
    return out,oof

# ---------- H4 ----------
H4={}; H4p={}
for blk,g in [('Overall',d),('US',d[d.corpus_id=='US']),('PAK',d[d.corpus_id=='PAK'])]:
    g=g.reset_index(drop=True); res,_=cv_eval(g,['M1','M2','M3','M4'])
    H4[blk]={m:(np.mean(v),np.std(v,ddof=1),v) for m,v in res.items()}
    comps=[]
    for a_,b_ in [('M2','M1'),('M3','M2'),('M4','M3')]:
        dd=np.array(res[a_])-np.array(res[b_]); t,pv=stats.ttest_1samp(dd,0)
        h=stats.t.ppf(.975,4)*dd.std(ddof=1)/np.sqrt(5)
        comps.append(dict(cmp=f'{a_}-{b_}',diff=dd.mean(),lo=dd.mean()-h,hi=dd.mean()+h,p=pv))
    for r,q in zip(comps,bh([c['p'] for c in comps])): r['p_bh']=q
    H4p[blk]=comps
R['H4']={k:{m:(v[0],v[1]) for m,v in x.items()} for k,x in H4.items()}; R['H4_folds']={k:{m:v[2] for m,v in x.items()} for k,x in H4.items()}; R['H4_cmp']=H4p

# ---------- H5 ----------
def metrics(y,p): 
    yh=(p>=.5).astype(int); return accuracy_score(y,yh),f1_score(y,yh,average='macro'),roc_auc_score(y,p)
def boot(y,p,B=1000):
    y=np.asarray(y); p=np.asarray(p); r=np.random.default_rng(SEED); out=[]
    for _ in range(B):
        i=r.integers(0,len(y),len(y))
        if len(np.unique(y[i]))<2: continue
        out.append(metrics(y[i],p[i]))
    return np.percentile(np.array(out),[2.5,97.5],axis=0)
def transfer(data,grouped=False,label='primary'):
    US=data[data.corpus_id=='US'].reset_index(drop=True); PK=data[data.corpus_id=='PAK'].reset_index(drop=True)
    cells={}
    for nm,g in [('US->US',US),('PAK->PAK',PK)]:
        _,oof=cv_eval(g,['TX'],grouped=grouped); p=oof['TX']; cells[nm]=(g.y.values,p)
    cells['US->PAK']=(PK.y.values,build('TX',US,PK)); cells['PAK->US']=(US.y.values,build('TX',PK,US))
    out={}
    for k,(y,p) in cells.items():
        m=metrics(y,p); ci=boot(y,p); out[k]=dict(acc=m[0],f1=m[1],auc=m[2],ci=ci.tolist(),n=len(y),
            maj=max(y.mean(),1-y.mean()))
    return out
R['H5']=transfer(d)
# ---------- sensitivity ----------
sens={}
# S1: rule-based corpus labels, deduplicated
d1=d.copy(); d1['corpus_id']=d1.corpus_rule
sens['S1_rule_labels']=dict(n_pak=int((d1.corpus_id=='PAK').sum()),H5=transfer(d1))
r1,_=cv_eval(d1[d1.corpus_id=='PAK'].reset_index(drop=True),['M2','M3','M4']); sens['S1_rule_labels']['PAK_H4']={m:np.mean(v) for m,v in r1.items()}
# S2: duplicates kept, ordinary folds (leaky); S3: duplicates kept, grouped folds
f2=full.copy()
for nm,gr in [('S2_dups_naive',False),('S3_dups_grouped',True)]:
    pk=f2[f2.corpus_id=='PAK'].reset_index(drop=True); us=f2[f2.corpus_id=='US'].reset_index(drop=True)
    rp,_=cv_eval(pk,['M2','M3','M4'],grouped=gr)
    sens[nm]=dict(n_pak=len(pk),PAK_H4={m:np.mean(v) for m,v in rp.items()},H5=transfer(f2,grouped=gr))
R['sens']=sens
json.dump(R,open('results.json','w'),default=lambda o: o.tolist() if hasattr(o,'tolist') else str(o),indent=1)
d.to_csv('pakliar_clean.csv',index=False)
print('done')
