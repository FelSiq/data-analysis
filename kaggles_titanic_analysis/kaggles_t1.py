import csv as csv;
import numpy as np;

csv_data = csv.reader(open('./data/train.csv', 'rb'));
header = csv_data.next();

#Convert the CSV data into a python list
data = []
for k in csv_data:
	data.append(k);
data = np.array(data);

#Raw data
number_of_passengers = np.size(data[0::,1].astype(np.float));
number_survived = np.sum(data[0::,1].astype(np.float));
proportion = number_survived/number_of_passengers; 

print 'No. passengers:\t'+str(number_of_passengers)+'\tNo. survived:\t'+str(number_survived);
print 'The proportion of the survivors is:\t' + str(round(proportion, 3));

del (number_of_passengers);
del (number_survived);
del (proportion);

#Gender issues
#Verify the diff between males and females
pass_female = data[0::,4] == 'female';
pass_male = data[0::,4] != 'female';

survived_female = np.sum(data[pass_female, 1].astype(np.float));
survived_male = np.sum(data[pass_male, 1].astype(np.float));

print '\nGender\t\tTotal:\t\tSurvived:\tRate:'
print 'Female:\t\t' + str(np.sum(pass_female)) + '\t\t'+ str(int(survived_female))+'\t\t' + str(round(survived_female/np.sum(pass_female),5));
print 'Male:\t\t' + str(np.sum(pass_male)) + '\t\t'+ str(int(survived_male)) + '\t\t'+ str(round(survived_male/np.sum(pass_male),5));

#We already know that the title is extremely important. In fact, the title kinda 
#merge the gender information with the class and the age, important factors of
#prediction in this situation. # Let's extract then.
import operator;
import re;
from collections import Counter;

titles = [];
for k in data[0::,3]:
	titles.append(re.split(r'\,\ (.*?)\ ', k)[1]);

counter_obj = Counter(titles);

print '\nHidden variable "Class" check-up:';
for k in counter_obj:
	print '%s: %s' % (k, counter_obj[k]);
"""
	Miss.: 182
	Mme.: 1
	Rev.: 6
	Jonkheer.: 1
	Sir.: 1
	Mlle.: 2
	Mrs.: 125
	Capt.: 1
	Col.: 2
	Ms.: 1
	Mr.: 517
	Lady.: 1
	Dr.: 7
	the: 1
	Master.: 40
	Major.: 2
	Don.: 1
"""
#Some titles seens to be 'weird', like the 'the' one. 
#Thus, some of then seens likely to be merged into a single one more numerous.

#1. 'the'?
print '\nTitle "the" analysis:\n' + str(data[titles.index('the')]) + '\n';
#Seens like the right title should be 'Countess'. Switch it to Mistress.
titles[titles.index('the')] = 'Mrs.';

#Append the new information on the data matrix
#print str(np.shape(data)) + '/'+ str(np.shape(titles));
titles = np.array(titles)[..., None];
data1 = np.append(data, titles, axis=1);

#2. Rev and Dr... Those titles has a small yet expressive number of passengers. 
#Whats the proportion of survival compared to misters, in general?
passenger_doc = (data1[0::, -1] == 'Dr.');
passenger_rev = (data1[0::, -1] == 'Rev.');

print '"Doctor" and "Reverend" class lookup:\nDr.: %lf%%\tRev.: %lf%%\n' % (100*np.sum(data1[passenger_doc, 1].astype(np.float))/counter_obj['Dr.'], 
	100*np.sum(data1[passenger_rev, 1].astype(np.float))/counter_obj['Rev.']);
#Bad news for reverends, but still a good chance for doctors.
#Let's merge revereds into Misters.

data1[passenger_rev, -1] = 'Mr.';
del(passenger_rev);

#3. Merge every title that is not "mr", "miss", "master", "mrs" on a single one called "other".
other_titles = np.logical_and(np.logical_and(data1[0::, 12] != 'Mrs.', data1[0::, 12] != 'Mr.'), 
	np.logical_and(data1[0::, 12] != 'Master.', data1[0::, 12] != 'Miss.'));
data1[other_titles, 12] = 'Other.';

#Check new title data.
counter_obj = Counter(data1[0::, 12]);
print 'New "Title" data:';
for k in counter_obj:
	print '%s: %s' % (k, counter_obj[k]);
#Everything seens OK.

#Check up the class thing.
fst_class_passenger = (data1[0::, 2] == '1');
snd_class_passenger = (data1[0::, 2] == '2');
trd_class_passenger = (data1[0::, 2] == '3');

print '\nRelative proportion of survival between the classes:'
print '1st: %.4lf\t2nd: %.4lf\t3rd: %.4lf\n' % (np.sum(data1[fst_class_passenger, 1].astype(np.float))/np.size(data1[fst_class_passenger, 1]), 
	(np.sum(data1[snd_class_passenger, 1].astype(np.float)))/np.size(data1[snd_class_passenger, 1]), 
	(np.sum(data1[trd_class_passenger, 1].astype(np.float)))/np.size(data1[trd_class_passenger, 1]));

del fst_class_passenger;
del snd_class_passenger;
del trd_class_passenger;
#Indeed, larger class passengers has higher probabilities of survival.

#Let's find out possible missing values on data.
import pandas as pd;

data_frame = pd.DataFrame(data=data1[0::,1::],
	index=data1[0::, 0],
	columns=('Survived', 'PClass', 'Name', 'Sex', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked', 'Title'));

data_frame[['Survived', 'PClass', 'Age', 'SibSp', 'Parch', 'Fare']] = data_frame[['Survived', 'PClass', 'Age', 'SibSp', 'Parch', 'Fare']].apply(lambda x: pd.to_numeric(x, errors='coerce'));
#print data_frame.head(10);
#data_frame.info();


#Now, let's examine a bit further.
print '\nClass x Sex survivability:\n\t\tMale [S/T/P]:\t\tFemale [S/T/P]'
for i in range(1,4):
	male_fc_total = np.size(data_frame[np.logical_and(data_frame['Sex'] == 'male', data_frame['PClass'] == i)][['Survived']]);
	male_fc_survived = np.sum(data_frame[np.logical_and(data_frame['Sex'] == 'male', data_frame['PClass'] == i)][['Survived']]);
	female_fc_total = np.size(data_frame[np.logical_and(data_frame['Sex'] == 'female', data_frame['PClass'] == i)][['Survived']]);
	female_fc_survived = np.sum(data_frame[np.logical_and(data_frame['Sex'] == 'female', data_frame['PClass'] == i)][['Survived']]);
	print 'Class %d:\t%u/%u/%.4lf\t\t%u/%u/%.4lf' % (i, male_fc_survived, male_fc_total, 100*(male_fc_survived/male_fc_total),
		female_fc_survived, female_fc_total, 100*(female_fc_survived/female_fc_total));
#Womans on 1st or 2nd class has almost a guaranteed survivability. 
#Men, in general, has low probability of survival.

#Let's look at the 'Master' title.
print '\nClass x Master survivability:'
for i in range(1,4):
	master_fc_total = np.size(data_frame[np.logical_and(data_frame['Title'] == 'Master.', data_frame['PClass'] == i)][['Survived']]);
	master_fc_survived = np.sum(data_frame[np.logical_and(data_frame['Title'] == 'Master.', data_frame['PClass'] == i)][['Survived']]);
	print 'Class %d:\t%u/%u/%.4lf' % (i, master_fc_survived, master_fc_total, 100*(master_fc_survived/master_fc_total));
#Masters on 1st and 2nd class always survives, however, on 3rd class, the odds are low.

#import pylab as pl;
#data_frame['Age'].hist();
#pl.show();

#Transform the 'Sex' and 'Embarked' thing onto a numeral representation.
#First, check NaN values:

print 'Checking for NA values on "Sex" and "Embarked" data:'
print data_frame[data_frame['Sex'].isnull()];
print data_frame[data_frame['Embarked'] == ''];
#There's two NAs on "Embarked". "Sex" seens already fine.
data_frame['Sex'] = data_frame['Sex'].map({'female': 0, 'male': 1}).astype(int);

#Get the most frequent of "Embarked" and assume this value on NAs.
data_frame.loc[data_frame['Embarked'] == '', 'Embarked'] = Counter(data_frame['Embarked']).most_common(1)[0][0];

#Same thing with the embarked values, this time S: 0, C: 1, Q:2;
print '\nUnique "Embarked" options: %s' % pd.unique(data_frame['Embarked'].values.ravel());
data_frame['Embarked'] = data_frame['Embarked'].map({'S': 0, 'C': 1, 'Q': 2}).astype(int);

#Now let's get rid of the NA's values on all the age data,
#assuming the value of then is the median value.
#median = np.median(data_frame['Age']);

data_frame.loc[data_frame['Age'].isnull(), 'Age'] = data_frame['Age'].median(axis='rows', skipna = True);

#Check if all stuff worked.
print '\nNew "Sex", "Age" and "Embarked" numeral representation:\n' + str(data_frame[['Name','Sex', 'Age', 'Embarked']].head(15));
print data_frame.describe();