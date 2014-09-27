import csv
import re
import nltk
 
 
def notNone(s,d):
    s = s.strip()
    d = d.strip()
    if s == '': return d
    else: return s
 
 
def getJobFunction(txt):
        txt = txt.lower()
        txt = re.sub('[^A-Za-z ]+', ' ', txt)
        txt = re.sub("\s\s+" , " ", txt)
        if len(txt.split()) > 11 or len(txt.split())==0: return None
 
 
        txt = txt.strip()
 
        if re.search(r"""(retired|former|no\slonger|^ex\b)""", txt):
            return 'Other'
 
        elif re.search(r"""(consult)""", txt):
            return 'Consultant'
 
        elif re.search(r"""(\bcfo\b|cfo|finance|financial|finan|credit|budget|bank|wealth|accounting|accountant|acountant|\bcpa\b|payable)""", txt):
            return 'Finance/Accounting'
 
        elif re.search(r"""(market|\bmkt|\bcmo\b)""", txt):
            return 'Marketing'
 
        elif (min([nltk.metrics.edit_distance('architect', item) for item in txt.split()]) < 3
                or re.search(r"""(programmer|programing|software|hardware|developer|developar|engi|engineer|research|improvement|data|design|archi|arquitecto|ecommerce|quantitative|\bmobile|analyst|analytics|analysis)""", txt)) \
                and not (re.search(r"""(\bit\b|\bict\b|compensation|network|account|\bacct\b)""", txt)):
            return 'Engineering'
 
        elif re.search(r"""(\bdba\b|\bsys\b|help\sdesk\sadmin|\bapp.+dev|system|tech|cloud|\bit\b|\bict\b|linux|unix\b|\blan\b|windows|computer|sap\sadmin|sysadmin|net\sadmin|network\sadmin|network|server|\bserver|info|\bweb\b|\bwebsite\b|\bcto\b|\bcio\b|infrastructure|digital)""", txt):
            return 'IT'
 
        elif re.search(r"""(\bhr\b|\bhuman\b|people\b|payroll|employer|employee\srelation|recruit|talent|benefits|compensation)""", txt):
            return 'HR'
 
        elif (min([nltk.metrics.edit_distance('account', item) for item in txt.split()]) < 3
                or re.search(r"""(\bsales\b|\bsale\b|acct\b|business dev|bus dev|biz dev)""", txt)):
            return 'Sales'
 
        elif re.search(r"""(opera\w+|\bop\b|\bops\b|manufact|factory)""", txt) \
                and not (re.search(r'''(\bsale|market|\bmkt\b)''', txt)):
            return 'Operations'
 
        elif re.search(r"""(secretary|secr|admin|administration|administrator|administrat|bookkeep)""", txt):
            return 'Administration'
 
        elif re.search(r"""(customer|\bcrm\b|community|representative|rep\s|client|consumer|\bpublic\b|relations|professional service)""", txt):
            return 'Customer Service and Support'
 
        elif re.search(r"""(support)""", txt):
            return 'Support'
 
        elif re.search(r"""(creative|media|advertisment|adverti)""", txt):
            return 'Creative'
 
        elif re.search(r"""(buyer|acquisition|purchas)""", txt):
            return 'Buyer'
 
        elif re.search(r"""(product\b|products\b)""", txt):
            return 'Product'
 
        elif re.search(r"""(medical|dentist|clinic|hospital|doctor|\bdr\b|physician|pharm|therapist|nurse|health)""", txt):
            return 'Medical'
 
        elif re.search(r"""(legal|judge|counsel|attorney|lawyer)""", txt):
            return 'Legal'
 
        elif re.search(r"""(\bexec|\bceo\b|founder|president|owner|chair|\bgm|general\w+manager)""", txt) \
                and not re.search(r"""(\bson\b|daughter|wife|husband|brother|sister|friend)""", txt):
            return 'General Management'
 
        return 'Other'
 
 
def getSeniority(txt):
 
        txt = txt.lower()
        txt = re.sub('[^A-Za-z ]+', ' ', txt)
        txt = re.sub("\s\s+" , " ", txt)
        if len(txt.split()) > 11 or len(txt.split())==0: return None
 
 
        txt = txt.strip()
        if re.search(r"""(retired|former|no\slonger|^ex\b)""", txt):
            return 'Other'
 
        elif re.search(r"""(\bboard|chair)""", txt) \
                and not re.search(r"""(clerk|group|commissioner|region|anal|service|vistage|practice|\bit\b|deputy|designate)""", txt):
            return 'Board'
 
        elif re.search(r"""(\bc.o\b|ceo|chief.*offic|cheif.*offic|c level)""",txt) \
                and not (re.search(r"""(director|dir|dr\sof|advisor|exec.*vp|ass.*c.o|ass|admin|admin.*c.o|account|coach)""", txt)):
            return 'CXO'
 
        elif (min([nltk.metrics.edit_distance('entrepeneur', item) for item in txt.split()]) < 3
                or re.search(r"""(owner|founder|proprietor)""", txt)) \
                and not (re.search(r"""(account|\bacct\b|\bson\b|daughter|wife|husband|brother|sister|friend)""", txt)):
            return 'Owner'
 
        elif (min([nltk.metrics.edit_distance('president', item) for item in txt.split()]) < 3
                or re.search(r"""(v ?p|\bsvp\b|vice\s?presid|head|\bgm|general\w+manager|principal|prinicipal|principle|dean|managing partner|executive|exec\s)""", txt)) \
                and not (re.search(r"""(account\sexec|acct\sexec|acccount\sexec|account\smanager|account\smanger|exgm|director)""", txt)):
            return 'VP'
 
        elif re.search(r"""(director|dir|dr\sof|controller)""", txt) and not (re.search(r"""(account)""", txt)):
            return 'Director'
 
        elif (min([nltk.metrics.edit_distance('manager', item) for item in txt.split()]) < 3
                or re.search(r"""(\bmgr\b|\bmngr\b|supervisor|superintendent|supv|influencer|lead)""", txt)) \
                and not (re.search(r"""(offic|accou|\bacct\b)""", txt)):
            return 'Manager'
 
        elif re.search(r"""(senior|\bsnr\b|\bsr\b)""", txt):
            return 'Senior'
 
        elif re.search(r"""(partner|advisor|adviser|trustee|consultant)""", txt) and not re.search(r"""(.+consultant)""", txt):
            return 'Partner'
 
        elif re.search(r"volunteer", txt):
            return 'Volunteer'
 
        elif re.search(r"""(student|intern\b|stiudent|trainee)""", txt):
            return 'Intern'
 
        elif re.search(r"""(\bjr\b|\bjunior\b|analyst|auditor|consu|rep|facilitator|trainer|research|clerk|recuite|secretary|account|acct|liaison|distributor|recruiter|assistant|asst|coordinat|worker|liason|support|contact|marketeer|trafficker|cfp|planner|specialist|associate|broker|agent|counsel|attorney|lawyer|legal|strategist|stategist|investigator|treasurer|practitioner|coach|appraiser|marketer|banker|professional|designer|businessman|cpa|investor|estimator|commissioner|buyer|purchaser|acct|writer|economist|pubilsher|publisher|designer|practictioner|producer|artist|librarian|editor|expert|integrator|generalist|physician|pharmacist|therapist|\bmd\b|catalyst|\beng\b|\bengi|employee|architect|dba|admin|tech|webmaster|programmer|program|evangelist|\bdev)""", txt):
            return 'Non-Management'
 
        return 'Other'
 
#--------------------------------
f = open('/Users/Fliptop/IdeaProjects/Classification/stuff2/lead_activity_features_9017947.csv','rb')
 
o = open('/Users/Fliptop/IdeaProjects/Classification/stuff2/lead_activity_features_9017947_OUT_test.csv','wb')
reader = csv.reader(f,delimiter=',')
writer = csv.writer(o,delimiter=',')
 
ctr = 0
bad_ctr = 0
 
for row in reader:
    ctr += 1
    if ctr == 1:
        header = row
        header.append('JobSeniority')
        header.append('JobFunction')
        writer.writerow(header)
        continue
    try:
        PG_Occupation = row[167]
        Platform_title = row[236]
    except IndexError as ie:
        bad_ctr += 1
        continue
 
 
    combined = notNone(PG_Occupation, Platform_title)
    job_function = getJobFunction(combined)
    job_seniority = getSeniority(combined)
    row.append(job_seniority)
    row.append(job_function)
    writer.writerow(row)
 
 
print 'total bad is %d' %bad_ctr
f.close()
o.close()
