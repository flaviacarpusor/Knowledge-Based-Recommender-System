# Knowledge-Based Recommender System

A Python-based “What am I thinking of?” style game and recommender system using an existing dataset, in our case Music dataset.  
It provides three main functionalities:

1. **Item Identification** – Asks a series of questions to determine the unique item matching all answers.
2. **User Evaluation** – Asks questions about an item and scores the user based on correct answers.
3. **Item Recommendation** – Given at least 3 items, recommends similar ones, highlighting similarities and significant differences.

Datasets are preprocessed by splitting multi-value attributes, normalizing numeric values, and clustering when needed. Non-relevant attributes can be ignored.
