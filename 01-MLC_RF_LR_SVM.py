"""
=================================
1. Load module
=================================
"""
## base module 
import numpy as np
import pandas as pd
from sklearn import metrics
from sklearn.model_selection import train_test_split
from sklearn.model_selection import KFold,StratifiedKFold
## evaluation mudule
from sklearn.metrics import hamming_loss
from sklearn.metrics import accuracy_score
from sklearn.metrics import f1_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
from sklearn.metrics import zero_one_loss
from sklearn.metrics import jaccard_score

### classifier
from skmultilearn.dataset import load_dataset
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from skmultilearn.problem_transform import BinaryRelevance
from skmultilearn.problem_transform import ClassifierChain
from skmultilearn.problem_transform import LabelPowerset
from skmultilearn.adapt import MLkNN
from sklearn.naive_bayes import GaussianNB
from skmultilearn.ensemble import RakelD
from skmultilearn.ensemble import RakelO
from skmultilearn.ensemble import MajorityVotingClassifier
from skmultilearn.cluster import FixedLabelSpaceClusterer
from skmultilearn.ensemble import LabelSpacePartitioningClassifier
from sklearn.cluster import KMeans
from skmultilearn.cluster import MatrixLabelSpaceClusterer
from sklearn.multiclass import OneVsRestClassifier


"""
===========================================================
2. Load data
===========================================================
"""

## gi_data
gi_data = pd.read_csv("cip_ctx_ctz_gen_gi_data.csv",index_col=0)
gi_pheno=pd.read_csv("cip_ctx_ctz_gen_gi_pheno.csv",index_col=0)
gi_data.shape,gi_pheno.shape
gi_data2 = gi_data.values
gi_pheno2 = gi_pheno.values
gi_data2.shape,gi_pheno2.shape

#x_train,x_test,y_train,y_test=train_test_split(gi_data2,gi_pheno2,test_size=0.2,random_state=123)
X = gi_data2
y = gi_pheno2

## pub_data
pub_data = pd.read_csv("cip_ctx_ctz_gen_pub_data.csv",index_col=0)
pub_pheno=pd.read_csv("cip_ctx_ctz_gen_pub_pheno.csv",index_col=0)
pub_data.shape,pub_pheno.shape
pub_data2 = pub_data.values
pub_pheno2 = pub_pheno.values
pub_data2.shape,pub_pheno2.shape

X_test= pub_data2
y_test= pub_pheno2

"""
===========================================================
3. Method 1: Binary Relevance
===========================================================
# class skmultilearn.problem_transform.BinaryRelevance(classifier=None, require_dense=None)
"""
classifier = BinaryRelevance(classifier = RandomForestClassifier(),
require_dense = [False, True])

# train
classifier.fit(X, y)

# predict
prediction = classifier.predict(X_test)

# print classifiers info
print(classifier.model_count_)
print(classifier.partition_)
print(classifier.classifiers_)

### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

### one vs rest classifier
LogReg_pipeline = Pipeline([
                ('clf', OneVsRestClassifier(LogisticRegression(solver='sag'), n_jobs=-1)),
            ])
for category in categories:
    print('**Processing {} comments...**'.format(category))
    
    # Training logistic regression model on train data
    LogReg_pipeline.fit(x_train, train[category])
    
    # calculating test accuracy
    prediction = LogReg_pipeline.predict(x_test)
    print('Test accuracy is {}'.format(accuracy_score(test[category], prediction)))
    print("\n")

"""
===========================================================
3. Method 2: Classifier Chains
===========================================================
# class skmultilearn.problem_transform.ClassifierChain(classifier=None, require_dense=None, order=None)
"""
classifier = ClassifierChain(classifier = RandomForestClassifier(),
require_dense = [False, True],order=[3,2,1,0])

# train
classifier.fit(X, y)

# predict
predictions = classifier.predict(X_test)

# print classifiers info
print(classifier.classifiers_)


### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)


"""
===========================================================
3. Method 3: Ensemble Classifier Chains （ECC）
===========================================================
# https://github.com/ChristianSch/skml/blob/master/skml/ensemble/ensemble_classifier_chains.py
"""
import random
import numpy as np
from sklearn.base import BaseEstimator, MetaEstimatorMixin, ClassifierMixin
from sklearn.utils import validation
from skmultilearn.problem_transform import ClassifierChain

class EnsembleClassifierChain(
 BaseEstimator, MetaEstimatorMixin, ClassifierMixin):
 def __init__(
     self,
     estimator,
     number_of_chains=24,
     threshold=.5,
     max_features=1.0):
     self.number_of_chains = number_of_chains
     self.estimator = estimator
     self.threshold = threshold
     self.max_features = max_features
     self.estimators_ = []
 def fit(self, X, y):
     validation.check_X_y(X, y, multi_output=True)
     y = validation.check_array(y, accept_sparse=True)
     for i in range(self.number_of_chains):
            # the classifier gets cloned internally in classifer chains, so
            # no need to do that here.
         cc = ClassifierChain(self.estimator)
         no_samples = y.shape[0]
            # create random subset for each chain individually
         idx = random.sample(range(no_samples),
                                int(no_samples * self.max_features))
         cc.fit(X[idx, :], y[idx, :])
         self.estimators_.append(cc)
 def predict(self, X):
     validation.check_is_fitted(self, 'estimators_')
     preds = np.array([cc.predict(X) for cc in self.estimators_])
     preds = np.sum(preds, axis=0)
     W_norm = preds.mean(axis=0)
     out = preds / W_norm
     return (out >= self.threshold).astype(int)
     
     
classifier = EnsembleClassifierChain(RandomForestClassifier())

# train
X = X.toarray()
classifier.fit(X, y)

# predict
X_test = X_test.toarray()
predictions = classifier.predict(X_test)


### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

"""
===========================================================
3. Method 4: Label Powerset
===========================================================
# class skmultilearn.problem_transform.LabelPowerset(classifier=None, require_dense=None)
"""

classifier = LabelPowerset(classifier = RandomForestClassifier(),require_dense = [False, True])

# train
classifier.fit(X, y)

# predict
predictions = classifier.predict(X_test)

# print classifiers info
print(classifier.unique_combinations_)
print(classifier.reverse_combinations_)

### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

"""
============================================================================================
Ensembles of classifiers
============================================================================================
"""

"""
===========================================================
3. Method 5: RAkELd: random label space partitioning with Label Powerset
===========================================================
# class skmultilearn.ensemble.RakelD(base_classifier=None, labelset_size=3, base_classifier_require_dense=None)
"""


classifier = RakelD(
    base_classifier=GaussianNB(),
    base_classifier_require_dense=[True, True],
    labelset_size=6
)

classifier.fit(X, y)
prediction = classifier.predict(X_test)

# print classifiers info
print(classifier.model_count_)
print(classifier._label_count)
print(classifier.classifier_)

### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

"""
===========================================================
3. Method 6: RAkELo: random overlapping label space division with Label Powerset
===========================================================
# class skmultilearn.ensemble.RakelO(base_classifier=None, model_count=None, labelset_size=3, base_classifier_require_dense=None)
"""


# classifier = RakelO(
    # base_classifier=GaussianNB(),
    # base_classifier_require_dense=[True, True],
    # labelset_size=y.shape[1] // 4,
    # model_count =6
# )

# classifier.fit(X, y)
# prediction = classifier.predict(X_test)

# # print classifiers info
# print(classifier.model_count_)
# print(classifier._label_count)
# print(classifier.classifier_)




"""
===========================================================
3. Method 7: Network-based label space partition ensemble classification
===========================================================
# class skmultilearn.ensemble.LabelSpacePartitioningClassifier(classifier=None, clusterer=None, require_dense=None)
"""

classifier = MajorityVotingClassifier(
    clusterer = FixedLabelSpaceClusterer(clusters = [[1,3,4], [0, 2, 5]]),
    classifier = ClassifierChain(classifier=GaussianNB())
)
classifier.fit(X,y)
predictions = classifier.predict(X_test)

# print classifiers info
print(classifier.model_count_)
print(classifier.partition_)
print(classifier.classifiers_)

### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

"""
===========================================================
3. Method 8: skmultilearn.ensemble.voting module
===========================================================
# class skmultilearn.ensemble.MajorityVotingClassifier(classifier=None, clusterer=None, require_dense=None)
"""
classifier = MajorityVotingClassifier(
    clusterer = FixedLabelSpaceClusterer(clusters = [[1,2,3], [0, 2, 1], [1, 2]]),
    classifier = ClassifierChain(classifier=GaussianNB())
)
classifier.fit(X,y)
predictions = classifier.predict(X_test)

# print classifiers info
print(classifier.model_count_)
print(classifier.partition_)
print(classifier.classifiers_)

### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

"""
============================================================================================
Label Space Clusterers
============================================================================================
"""

"""
===========================================================
3. Method 9: skmultilearn.cluster.fixed module
===========================================================
#class skmultilearn.cluster.FixedLabelSpaceClusterer(clusters=None)
"""

classifier = LabelSpacePartitioningClassifier(
    classifier = LabelPowerset(
        classifier=RandomForestClassifier(n_estimators=100),
        require_dense = [False, True]),
    require_dense = [True, True],
    clusterer = FixedLabelSpaceClusterer(clusters=[[1,2,3], [0,3]])
)

# train
classifier.fit(X, y)

# predict
predictions = classifier.predict(X_test)

### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

"""
===========================================================
3. Method 10: skmultilearn.cluster.matrix module
===========================================================
#class skmultilearn.cluster.MatrixLabelSpaceClusterer(clusterer=None, pass_input_space=False)
"""

# construct base forest classifier
base_classifier = RandomForestClassifier(n_estimators=1030)

# setup problem transformation approach with sparse matrices for random forest
problem_transform_classifier = LabelPowerset(classifier=base_classifier,
    require_dense=[False, False])

# setup the clusterer
clusterer = MatrixLabelSpaceClusterer(clusterer=KMeans(n_clusters=3))

# setup the ensemble metaclassifier
classifier = LabelSpacePartitioningClassifier(problem_transform_classifier, clusterer)

# train
classifier.fit(X, y)

# predict
predictions = classifier.predict(X_test)

### evaluation
hamming_loss(y_test, prediction)
accuracy_score(y_test, prediction)
jaccard_score(y_test, prediction, average='samples')
jaccard_score(y_test, prediction, average='macro')
jaccard_score(y_test, prediction, average='micro')
jaccard_score(y_test, prediction, average=None)
f1_score(y_test, prediction,average='micro')
f1_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='macro')
precision_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='micro')
recall_score(y_test, prediction,average='macro')
zero_one_loss(y_test, prediction,normalize=True)

"""
============================================================================================
External classifiers
============================================================================================
"""

"""
===========================================================
3. Method 11: skmultilearn.ext.keras module
===========================================================
#class skmultilearn.ext.Keras(build_function, multi_class=False, keras_params=None)[source]¶
Bases: sklearn.base.BaseEstimator
"""
# Single-class Keras classifier
from keras.models import Sequential
from keras.layers import Dense

def create_model_single_class(input_dim, output_dim):
    # create model
    model = Sequential()
    model.add(Dense(12, input_dim=input_dim, activation='relu'))
    model.add(Dense(8, activation='relu'))
    model.add(Dense(output_dim, activation='sigmoid'))
    # Compile model
    model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model
    
from skmultilearn.problem_transform import BinaryRelevance
from skmultilearn.ext import Keras

KERAS_PARAMS = dict(epochs=10, batch_size=100, verbose=0)

clf = BinaryRelevance(classifier=Keras(create_model_single_class, False, KERAS_PARAMS), require_dense=[True,True])
clf.fit(X, y)
result = clf.predict(X_test)

# Multi-class Keras classifier
def create_model_multiclass(input_dim, output_dim):
    # create model
    model = Sequential()
    model.add(Dense(8, input_dim=input_dim, activation='relu'))
    model.add(Dense(output_dim, activation='softmax'))
    # Compile model
    model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])
    return model
    
from skmultilearn.problem_transform import LabelPowerset
clf = LabelPowerset(classifier=Keras(create_model_multiclass, True, KERAS_PARAMS), require_dense=[True,True])
clf.fit(X,y)
y_pred = clf.predict(X_test)    


"""
===========================================================
4. Whole pipeline
===========================================================
"""
rf_BR = BinaryRelevance(RandomForestClassifier(n_estimators=200, random_state=0))
rf_OR = OneVsRestClassifier(RandomForestClassifier(n_estimators=200, random_state=0))
rf_CC = ClassifierChain(RandomForestClassifier(n_estimators=200, random_state=0))
rf_ECC = EnsembleClassifierChain(RandomForestClassifier(n_estimators=200, random_state=0))
rf_LP = LabelPowerset(RandomForestClassifier(n_estimators=200, random_state=0))
rf_RD = RakelD(RandomForestClassifier(n_estimators=200, random_state=0), labelset_size=3)

### evaluation_metrics
#### BR
rf_BR_acc_out = []
rf_BR_loss_out = []
rf_BR_jaccard_out = []
rf_BR_f1_out = []
rf_BR_precision_out = []
rf_BR_recall_out = []
rf_BR_zero_one_loss_out = []
rf_BR_y_pred_out = []
rf_BR_y_pred_prob_out = []

#### OR
rf_OR_acc_out = []
rf_OR_loss_out = []
rf_OR_jaccard_out = []
rf_OR_f1_out = []
rf_OR_precision_out = []
rf_OR_recall_out = []
rf_OR_zero_one_loss_out = []
rf_OR_y_pred_out = []
rf_OR_y_pred_prob_out = []

#### CC
rf_CC_acc_out = []
rf_CC_loss_out = []
rf_CC_jaccard_out = []
rf_CC_f1_out = []
rf_CC_precision_out = []
rf_CC_recall_out = []
rf_CC_zero_one_loss_out = []
rf_CC_y_pred_out = []
rf_CC_y_pred_prob_out = []

#### ECC
rf_ECC_acc_out = []
rf_ECC_loss_out = []
rf_ECC_jaccard_out = []
rf_ECC_f1_out = []
rf_ECC_precision_out = []
rf_ECC_recall_out = []
rf_ECC_zero_one_loss_out = []
rf_ECC_y_pred_out = []

#### LP
rf_LP_acc_out = []
rf_LP_loss_out = []
rf_LP_jaccard_out = []
rf_LP_f1_out = []
rf_LP_precision_out = []
rf_LP_recall_out = []
rf_LP_zero_one_loss_out = []
rf_LP_y_pred_out = []
rf_LP_y_pred_prob_out = []
#### RD
rf_RD_acc_out = []
rf_RD_loss_out = []
rf_RD_jaccard_out = []
rf_RD_f1_out = []
rf_RD_precision_out = []
rf_RD_recall_out = []
rf_RD_zero_one_loss_out = []
rf_RD_y_pred_out = []
rf_RD_y_pred_prob_out = []

cv = KFold(n_splits=5)
for i, (train, test) in enumerate(cv.split(X, y)):
#### BR
    rf_BR.fit(X[train], y[train])
    rf_BR_y_pred = rf_BR.predict(X[test])
    rf_BR_y_pred_prob = rf_BR.predict_proba(X[test])[:,1]
    rf_BR_acc = accuracy_score(y[test],rf_BR_y_pred)
    rf_BR_loss = hamming_loss(y[test],rf_BR_y_pred)
    rf_BR_jaccard = jaccard_score(y[test],rf_BR_y_pred,average=None)
    rf_BR_f1 = f1_score(y[test],rf_BR_y_pred,average=None)
    rf_BR_precision = precision_score(y[test],rf_BR_y_pred,average=None)
    rf_BR_recall = recall_score(y[test],rf_BR_y_pred,average=None)
    rf_BR_zero_one_loss = zero_one_loss(y[test],rf_BR_y_pred,normalize=True)  
    rf_BR_acc_out.append(rf_BR_acc)
    rf_BR_loss_out.append(rf_BR_loss)
    rf_BR_jaccard_out.append(rf_BR_jaccard)
    rf_BR_f1_out.append(rf_BR_f1)
    rf_BR_precision_out.append(rf_BR_precision)
    rf_BR_recall_out.append(rf_BR_recall)
    rf_BR_zero_one_loss_out.append(rf_BR_zero_one_loss)  
    rf_BR_y_pred_out.append(rf_BR_y_pred) 
    rf_BR_y_pred_prob_out.append(rf_BR_y_pred_prob) 
#### OR
    rf_OR.fit(X[train], y[train])
    rf_OR_y_pred = rf_OR.predict(X[test])
    rf_OR_y_pred_prob = rf_OR.predict_proba(X[test])[:,1]
    rf_OR_acc = accuracy_score(y[test],rf_OR_y_pred)
    rf_OR_loss = hamming_loss(y[test],rf_OR_y_pred)
    rf_OR_jaccard = jaccard_score(y[test],rf_OR_y_pred,average=None)
    rf_OR_f1 = f1_score(y[test],rf_OR_y_pred,average=None)
    rf_OR_precision = precision_score(y[test],rf_OR_y_pred,average=None)
    rf_OR_recall = recall_score(y[test],rf_OR_y_pred,average=None)
    rf_OR_zero_one_loss = zero_one_loss(y[test],rf_OR_y_pred,normalize=True)  
    rf_OR_acc_out.append(rf_OR_acc)
    rf_OR_loss_out.append(rf_OR_loss)
    rf_OR_jaccard_out.append(rf_OR_jaccard)
    rf_OR_f1_out.append(rf_OR_f1)
    rf_OR_precision_out.append(rf_OR_precision)
    rf_OR_recall_out.append(rf_OR_recall)
    rf_OR_zero_one_loss_out.append(rf_OR_zero_one_loss)  
    rf_OR_y_pred_out.append(rf_OR_y_pred) 
    rf_OR_y_pred_prob_out.append(rf_OR_y_pred_prob)     
#### CC
    rf_CC.fit(X[train], y[train])
    rf_CC_y_pred = rf_CC.predict(X[test])
    rf_CC_y_pred_prob = rf_CC.predict_proba(X[test])[:,1]
    rf_CC_acc = accuracy_score(y[test],rf_CC_y_pred)
    rf_CC_loss = hamming_loss(y[test],rf_CC_y_pred)
    rf_CC_jaccard = jaccard_score(y[test],rf_CC_y_pred,average=None)
    rf_CC_f1 = f1_score(y[test],rf_CC_y_pred,average=None)
    rf_CC_precision = precision_score(y[test],rf_CC_y_pred,average=None)
    rf_CC_recall = recall_score(y[test],rf_CC_y_pred,average=None)
    rf_CC_zero_one_loss = zero_one_loss(y[test],rf_CC_y_pred,normalize=True)  
    rf_CC_acc_out.append(rf_CC_acc)
    rf_CC_loss_out.append(rf_CC_loss)
    rf_CC_jaccard_out.append(rf_CC_jaccard)
    rf_CC_f1_out.append(rf_CC_f1)
    rf_CC_precision_out.append(rf_CC_precision)
    rf_CC_recall_out.append(rf_CC_recall)
    rf_CC_zero_one_loss_out.append(rf_CC_zero_one_loss)
    rf_CC_y_pred_out.append(rf_CC_y_pred) 
    rf_CC_y_pred_prob_out.append(rf_CC_y_pred_prob) 
#### ECC
    rf_ECC.fit(X[train], y[train])
    rf_ECC_y_pred = rf_ECC.predict(X[test])
    rf_ECC_acc = accuracy_score(y[test],rf_ECC_y_pred)
    rf_ECC_loss = hamming_loss(y[test],rf_ECC_y_pred)
    rf_ECC_jaccard = jaccard_score(y[test],rf_ECC_y_pred,average=None)
    rf_ECC_f1 = f1_score(y[test],rf_ECC_y_pred,average=None)
    rf_ECC_precision = precision_score(y[test],rf_ECC_y_pred,average=None)
    rf_ECC_recall = recall_score(y[test],rf_ECC_y_pred,average=None)
    rf_ECC_zero_one_loss = zero_one_loss(y[test],rf_ECC_y_pred,normalize=True)   
    rf_ECC_acc_out.append(rf_ECC_acc)
    rf_ECC_loss_out.append(rf_ECC_loss)
    rf_ECC_jaccard_out.append(rf_ECC_jaccard)
    rf_ECC_f1_out.append(rf_ECC_f1)
    rf_ECC_precision_out.append(rf_ECC_precision)
    rf_ECC_recall_out.append(rf_ECC_recall)
    rf_ECC_zero_one_loss_out.append(rf_ECC_zero_one_loss)
    rf_ECC_y_pred_out.append(rf_ECC_y_pred)      
#### LP
    rf_LP.fit(X[train], y[train])
    rf_LP_y_pred = rf_LP.predict(X[test])
    rf_LP_y_pred_prob = rf_LP.predict_proba(X[test])[:,1]
    rf_LP_acc = accuracy_score(y[test],rf_LP_y_pred)
    rf_LP_loss = hamming_loss(y[test],rf_LP_y_pred)
    rf_LP_jaccard = jaccard_score(y[test],rf_LP_y_pred,average=None)
    rf_LP_f1 = f1_score(y[test],rf_LP_y_pred,average=None)
    rf_LP_precision = precision_score(y[test],rf_LP_y_pred,average=None)
    rf_LP_recall = recall_score(y[test],rf_LP_y_pred,average=None)
    rf_LP_zero_one_loss = zero_one_loss(y[test],rf_LP_y_pred,normalize=True)   
    rf_LP_acc_out.append(rf_LP_acc)
    rf_LP_loss_out.append(rf_LP_loss)
    rf_LP_jaccard_out.append(rf_LP_jaccard)
    rf_LP_f1_out.append(rf_LP_f1)
    rf_LP_precision_out.append(rf_LP_precision)
    rf_LP_recall_out.append(rf_LP_recall)
    rf_LP_zero_one_loss_out.append(rf_LP_zero_one_loss)
    rf_LP_y_pred_out.append(rf_LP_y_pred) 
    rf_LP_y_pred_prob_out.append(rf_LP_y_pred_prob) 
#### RD
    rf_RD.fit(X[train], y[train])
    rf_RD_y_pred = rf_RD.predict(X[test])
    rf_RD_y_pred_prob = rf_RD.predict_proba(X[test])[:,1]
    rf_RD_acc = accuracy_score(y[test],rf_RD_y_pred)
    rf_RD_loss = hamming_loss(y[test],rf_RD_y_pred)
    rf_RD_jaccard = jaccard_score(y[test],rf_RD_y_pred,average=None)
    rf_RD_f1 = f1_score(y[test],rf_RD_y_pred,average=None)
    rf_RD_precision = precision_score(y[test],rf_RD_y_pred,average=None)
    rf_RD_recall = recall_score(y[test],rf_RD_y_pred,average=None)
    rf_RD_zero_one_loss = zero_one_loss(y[test],rf_RD_y_pred,normalize=True)    
    rf_RD_acc_out.append(rf_RD_acc)
    rf_RD_loss_out.append(rf_RD_loss)
    rf_RD_jaccard_out.append(rf_RD_jaccard)
    rf_RD_f1_out.append(rf_RD_f1)
    rf_RD_precision_out.append(rf_RD_precision)
    rf_RD_recall_out.append(rf_RD_recall)
    rf_RD_zero_one_loss_out.append(rf_RD_zero_one_loss)
    rf_RD_y_pred_out.append(rf_RD_y_pred) 
    rf_RD_y_pred_prob_out.append(rf_RD_y_pred_prob) 
    
### save evaluation out

file = open("out/cip_ctx_ctz_gen_gi_out4.csv","w")
file.write(
    "rf_BR_acc_out:"+str(rf_BR_acc_out)+'\n'+"rf_BR_loss_out:"+str(rf_BR_loss_out)+'\n'+"rf_BR_jaccard_out:"+str(rf_BR_jaccard_out)+'\n'+"rf_BR_f1_out:"+str(rf_BR_f1_out)+'\n'+"rf_BR_precision_out:"+str(rf_BR_precision_out)+'\n'+"rf_BR_recall_out:"+str(rf_BR_recall_out)+'\n'+"rf_BR_zero_one_loss_out:"+str(rf_BR_zero_one_loss_out)+'\n'   
    "rf_OR_acc_out:"+str(rf_OR_acc_out)+'\n'+"rf_OR_loss_out:"+str(rf_OR_loss_out)+'\n'+"rf_OR_jaccard_out:"+str(rf_OR_jaccard_out)+'\n'+"rf_OR_f1_out:"+str(rf_OR_f1_out)+'\n'+"rf_OR_precision_out:"+str(rf_OR_precision_out)+'\n'+"rf_OR_recall_out:"+str(rf_OR_recall_out)+'\n'+"rf_OR_zero_one_loss_out:"+str(rf_OR_zero_one_loss_out)+'\n'   
    "rf_CC_acc_out:"+str(rf_CC_acc_out)+'\n'+"rf_CC_loss_out:"+str(rf_CC_loss_out)+'\n'+"rf_CC_jaccard_out:"+str(rf_CC_jaccard_out)+'\n'+"rf_CC_f1_out:"+str(rf_CC_f1_out)+'\n'+"rf_CC_precision_out:"+str(rf_CC_precision_out)+'\n'+"rf_CC_recall_out:"+str(rf_CC_recall_out)+'\n'+"rf_CC_zero_one_loss_out:"+str(rf_CC_zero_one_loss_out)+'\n' 
    "rf_ECC_acc_out:"+str(rf_ECC_acc_out)+'\n'+"rf_ECC_loss_out:"+str(rf_ECC_loss_out)+'\n'+"rf_ECC_jaccard_out:"+str(rf_ECC_jaccard_out)+'\n'+"rf_ECC_f1_out:"+str(rf_ECC_f1_out)+'\n'+"rf_ECC_precision_out:"+str(rf_ECC_precision_out)+'\n'+"rf_ECC_recall_out:"+str(rf_ECC_recall_out)+'\n'+"rf_ECC_zero_one_loss_out:"+str(rf_ECC_zero_one_loss_out)+'\n'   
    "rf_LP_acc_out:"+str(rf_LP_acc_out)+'\n'+"rf_LP_loss_out:"+str(rf_LP_loss_out)+'\n'+"rf_LP_jaccard_out:"+str(rf_LP_jaccard_out)+'\n'+"rf_LP_f1_out:"+str(rf_LP_f1_out)+'\n'+"rf_LP_precision_out:"+str(rf_LP_precision_out)+'\n'+"rf_LP_recall_out:"+str(rf_LP_recall_out)+'\n'+"rf_LP_zero_one_loss_out:"+str(rf_LP_zero_one_loss_out)+'\n'  
    "rf_RD_acc_out:"+str(rf_RD_acc_out)+'\n'+"rf_RD_loss_out:"+str(rf_RD_loss_out)+'\n'+"rf_RD_jaccard_out:"+str(rf_RD_jaccard_out)+'\n'+"rf_RD_f1_out:"+str(rf_RD_f1_out)+'\n'+"rf_RD_precision_out:"+str(rf_RD_precision_out)+'\n'+"rf_RD_recall_out:"+str(rf_RD_recall_out)+'\n'+"rf_RD_zero_one_loss_out:"+str(rf_RD_zero_one_loss_out)+'\n'
)
file.close()    

# test on public data
#### BR
rf_BR_y_pred_out = rf_BR.predict(pub_data2)
rf_BR_acc = accuracy_score(pub_pheno2,rf_BR_y_pred_out)
rf_BR_loss = hamming_loss(pub_pheno2,rf_BR_y_pred_out)
rf_BR_jaccard = jaccard_score(pub_pheno2,rf_BR_y_pred_out,average=None)
rf_BR_f1 = f1_score(pub_pheno2,rf_BR_y_pred_out,average=None)
rf_BR_precision = precision_score(pub_pheno2,rf_BR_y_pred_out,average=None)
rf_BR_recall = recall_score(pub_pheno2,rf_BR_y_pred_out,average=None)
rf_BR_zero_one_loss = zero_one_loss(pub_pheno2,rf_BR_y_pred_out,normalize=True) 

#### OR
rf_OR_y_pred_out = rf_OR.predict(pub_data2)
rf_OR_acc = accuracy_score(pub_pheno2,rf_OR_y_pred_out)
rf_OR_loss = hamming_loss(pub_pheno2,rf_OR_y_pred_out)
rf_OR_jaccard = jaccard_score(pub_pheno2,rf_OR_y_pred_out,average=None)
rf_OR_f1 = f1_score(pub_pheno2,rf_OR_y_pred_out,average=None)
rf_OR_precision = precision_score(pub_pheno2,rf_OR_y_pred_out,average=None)
rf_OR_recall = recall_score(pub_pheno2,rf_OR_y_pred_out,average=None)
rf_OR_zero_one_loss = zero_one_loss(pub_pheno2,rf_OR_y_pred_out,normalize=True) 

#### CC
rf_CC_y_pred_out = rf_CC.predict(pub_data2)
rf_CC_acc = accuracy_score(pub_pheno2,rf_CC_y_pred_out)
rf_CC_loss = hamming_loss(pub_pheno2,rf_CC_y_pred_out)
rf_CC_jaccard = jaccard_score(pub_pheno2,rf_CC_y_pred_out,average=None)
rf_CC_f1 = f1_score(pub_pheno2,rf_CC_y_pred_out,average=None)
rf_CC_precision = precision_score(pub_pheno2,rf_CC_y_pred_out,average=None)
rf_CC_recall = recall_score(pub_pheno2,rf_CC_y_pred_out,average=None)
rf_CC_zero_one_loss = zero_one_loss(pub_pheno2,rf_CC_y_pred_out,normalize=True) 


#### ECC
rf_ECC_y_pred_out = rf_CC.predict(pub_data2)
rf_ECC_acc = accuracy_score(pub_pheno2,rf_ECC_y_pred_out)
rf_ECC_loss = hamming_loss(pub_pheno2,rf_ECC_y_pred_out)
rf_ECC_jaccard = jaccard_score(pub_pheno2,rf_ECC_y_pred_out,average=None)
rf_ECC_f1 = f1_score(pub_pheno2,rf_ECC_y_pred_out,average=None)
rf_ECC_precision = precision_score(pub_pheno2,rf_ECC_y_pred_out,average=None)
rf_ECC_recall = recall_score(pub_pheno2,rf_ECC_y_pred_out,average=None)
rf_ECC_zero_one_loss = zero_one_loss(pub_pheno2,rf_ECC_y_pred_out,normalize=True) 


#### LP
rf_LP_y_pred_out = rf_LP.predict(pub_data2)
rf_LP_acc = accuracy_score(pub_pheno2,rf_LP_y_pred_out)
rf_LP_loss = hamming_loss(pub_pheno2,rf_LP_y_pred_out)
rf_LP_jaccard = jaccard_score(pub_pheno2,rf_LP_y_pred_out,average=None)
rf_LP_f1 = f1_score(pub_pheno2,rf_LP_y_pred_out,average=None)
rf_LP_precision = precision_score(pub_pheno2,rf_LP_y_pred_out,average=None)
rf_LP_recall = recall_score(pub_pheno2,rf_LP_y_pred_out,average=None)
rf_LP_zero_one_loss = zero_one_loss(pub_pheno2,rf_LP_y_pred_out,normalize=True) 

#### RD
rf_RD_y_pred_out = rf_RD.predict(pub_data2)
rf_RD_acc = accuracy_score(pub_pheno2,rf_RD_y_pred_out)
rf_RD_loss = hamming_loss(pub_pheno2,rf_RD_y_pred_out)
rf_RD_jaccard = jaccard_score(pub_pheno2,rf_RD_y_pred_out,average=None)
rf_RD_f1 = f1_score(pub_pheno2,rf_RD_y_pred_out,average=None)
rf_RD_precision = precision_score(pub_pheno2,rf_RD_y_pred_out,average=None)
rf_RD_recall = recall_score(pub_pheno2,rf_RD_y_pred_out,average=None)
rf_RD_zero_one_loss = zero_one_loss(pub_pheno2,rf_RD_y_pred_out,normalize=True)

file = open("out/cip_ctx_ctz_gen_pub_out4.csv","w")
file.write(
    "rf_BR_acc_out:"+str(rf_BR_acc)+'\n'+"rf_BR_loss_out:"+str(rf_BR_loss)+'\n'+"rf_BR_jaccard_out:"+str(rf_BR_jaccard)+'\n'+"rf_BR_f1_out:"+str(rf_BR_f1)+'\n'+"rf_BR_precision_out:"+str(rf_BR_precision)+'\n'+"rf_BR_recall_out:"+str(rf_BR_recall)+'\n'+"rf_BR_zero_one_loss_out:"+str(rf_BR_zero_one_loss)+'\n'   	
    "rf_OR_acc_out:"+str(rf_OR_acc)+'\n'+"rf_OR_loss_out:"+str(rf_OR_loss)+'\n'+"rf_OR_jaccard_out:"+str(rf_OR_jaccard)+'\n'+"rf_OR_f1_out:"+str(rf_OR_f1)+'\n'+"rf_OR_precision_out:"+str(rf_OR_precision)+'\n'+"rf_OR_recall_out:"+str(rf_OR_recall)+'\n'+"rf_OR_zero_one_loss_out:"+str(rf_OR_zero_one_loss)+'\n'   	
    "rf_CC_acc_out:"+str(rf_CC_acc)+'\n'+"rf_CC_loss_out:"+str(rf_CC_loss)+'\n'+"rf_CC_jaccard_out:"+str(rf_CC_jaccard)+'\n'+"rf_CC_f1_out:"+str(rf_CC_f1)+'\n'+"rf_CC_precision_out:"+str(rf_CC_precision)+'\n'+"rf_CC_recall_out:"+str(rf_CC_recall)+'\n'+"rf_CC_zero_one_loss_out:"+str(rf_CC_zero_one_loss)+'\n' 
    "rf_ECC_acc_out:"+str(rf_ECC_acc)+'\n'+"rf_ECC_loss_out:"+str(rf_ECC_loss)+'\n'+"rf_ECC_jaccard_out:"+str(rf_ECC_jaccard)+'\n'+"rf_ECC_f1_out:"+str(rf_ECC_f1)+'\n'+"rf_ECC_precision_out:"+str(rf_ECC_precision)+'\n'+"rf_ECC_recall_out:"+str(rf_ECC_recall)+'\n'+"rf_ECC_zero_one_loss_out:"+str(rf_ECC_zero_one_loss)+'\n'   
    "rf_LP_acc_out:"+str(rf_LP_acc)+'\n'+"rf_LP_loss_out:"+str(rf_LP_loss)+'\n'+"rf_LP_jaccard_out:"+str(rf_LP_jaccard)+'\n'+"rf_LP_f1_out:"+str(rf_LP_f1)+'\n'+"rf_LP_precision_out:"+str(rf_LP_precision)+'\n'+"rf_LP_recall_out:"+str(rf_LP_recall)+'\n'+"rf_LP_zero_one_loss_out:"+str(rf_LP_zero_one_loss)+'\n'  
    "rf_RD_acc_out:"+str(rf_RD_acc)+'\n'+"rf_RD_loss_out:"+str(rf_RD_loss)+'\n'+"rf_RD_jaccard_out:"+str(rf_RD_jaccard)+'\n'+"rf_RD_f1_out:"+str(rf_RD_f1)+'\n'+"rf_RD_precision_out:"+str(rf_RD_precision)+'\n'+"rf_RD_recall_out:"+str(rf_RD_recall)+'\n'+"rf_RD_zero_one_loss_out:"+str(rf_RD_zero_one_loss)
) 
file.close() 

 