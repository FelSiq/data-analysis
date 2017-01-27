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
import re;
from collections import Counter;
titles = [];
for k in data[0::,3]:
	titles.append(re.split(r'\,\ (.*?)\ ', k)[1]);
print '\n';
counter_obj = Counter(titles);
for k in counter_obj:
	print str(k) + ': ' + str(counter_obj[k]);
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