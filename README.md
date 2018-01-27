# RHanLP ：R Interface of HanLP(Han Language Processing,version 1.5.3)

请先阅读[HanLP](https://github.com/hankcs/HanLP)相关文档和协议，加载所有数据和模型内存不少于3G

## 安装
 
 `$ git clone https://github.com/qxde01/RHanLP` 

根据[HanLP](https://github.com/hankcs/HanLP)的指引下载数据和模型文件，放到目录`RHanLP/inst/java`下,然后安装：
 
 `$ R CMD INSTALL RHanLP`
 
## 使用
### 分词:8种模式 

```
doc = "算法可大致分为基本算法、数据结构的算法、数论算法、计算几何的算法、图的算法、动态规划以及数值分析、加密算法、排序算法、检索算法、随机化算法、
并行算法、厄米变形模型、随机森林算法。\n算法可以宽泛的分为三类，\n一，有限的确定性算法，这类算法在有限的一段时间内终止。他们可能要花很长时间来执
行指定的任务，但仍将在一定的时间内终止。这类算法得出的结果常取决于输入值。\n二，有限的非确定算法，这类算法在有限的时间内终止。然而，对于一个（
或一些）给定的数值，算法的结果并不是唯一的或确定的。\n三，无限的算法，是那些由于没有定义终止定义条件，或定义的条件无法由输入的数据满足而不终
止运行的算法。通常，无限算法的产生是由于未能确定的定义终止条件。"
  
hanlp.segment(doc) ## 标准分词
hanlp.segment(doc,mode='CRF') ## CRF分词 
hanlp.segment(doc,mode='NER') ##开启命名实体识别的分词 
```
### 文本提取

```
hanlp.extractWords(text = doc, size = 10) ## 关键词提取
hanlp.extractPhrase(text = doc, size = 5L) ## 关键短语提取
hanlp.extractSummary(text = doc, size = 5L) ## 关键句子提取（文本摘要）
```

### 繁简转化
```
hanlp.convert(doc, mode = "t2s")
hanlp.convert(doc, mode = "s2t")
hanlp.convert(doc, mode = "s2hk")
```
### 依存分析
```
hanlp.parseDependency(text = doc, mode = "NN") ## 神经网络模型
hanlp.parseDependency(text = doc, mode = "CRF") ## CRF模型
hanlp.parseDependency(text = doc, mode = "MaxEnt") ## 最大熵模型
```
