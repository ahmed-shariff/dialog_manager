from nltk.classify.maxent import FunctionBackedMaxentFeatureEncoding
from nltk.classify.maxent import MaxentClassifier
import dill

class ME:
    """
    """
    def __init__(self,mappings):
        self._mappings = mappings;
        _labels = []
        for x,y in mappings:
            if x not in _labels:
                _labels.append(x)
        self._encoding = FunctionBackedMaxentFeatureEncoding(self._function,len(mappings),_labels)
        self._labels = _labels

    def _function(self,fset,label):
        _encoding = []
        #print(self._mappings)
        for fname,fval in fset.items():
            if (label,fname) in self._mappings:
                _encoding.append(self._mappings[label,fname])
        return _encoding

    def train(self,fset):
        self.classifier = MaxentClassifier.train(fset,encoding = self._encoding)   

    def get_prob_list(self,fset_dict):
        _prob_dict = self.classifier.prob_classify(fset_dict)
        return [(label,_prob_dict.prob(label)) for label in self._labels]

def get_maxent_list(fset,model_name):
    classifer_f = open(model_name+".pickle","rb")
    classifier = dill.load(classifer_f)
    classifer_f.close()
    return classifier.get_prob_list(fset)
    
