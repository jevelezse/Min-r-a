#!/usr/bin/python
import matplotlib.pyplot as plt
from wordcloud import WordCloud, STOPWORDS
import pandas as pd
df = pd.read_csv('/home/jennifer/Escritorio/tweets.csv')

# join tweets to a single string
words = ' '.join(df['x'])

# remove URLs, RTs, and twitter handles
no_urls_no_tags = " ".join([word for word in words.split()
                            if 'http' not in word
                                and not word.startswith('@')
                                and word != 'RT'
                            ])

from scipy.misc import imread

twitter_mask = imread('/home/jennifer/Descargas/twitter_mask.png', flatten=True)


wordcloud = WordCloud(
                      font_path='/home/jennifer/Descargas/cabin-sketch-v1.02/CabinSketch-Bold.ttf',
                      stopwords=STOPWORDS,
                      background_color='white',
                      width=1800,
                      height=1400,
                      mask=twitter_mask
            ).generate(no_urls_no_tags)

plt.imshow(wordcloud)
plt.axis("off")
plt.savefig('/home/jennifer/Escritorio/constopwords.png', dpi=300)
plt.show()