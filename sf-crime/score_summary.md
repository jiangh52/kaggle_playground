sf_crime

method						train_test_split_score
simple_prediction_v1		2.822216
knn_v1(k=100,cover_tree)	5.040006
knn_v1(k=100,kd_tree)	5.039261
knn_v1(k=50,cover_tree)		7.210072
knn_v1(k=20,cover_tree)		12.30715
knn_v2(k=100,kd_tree,scale_ratio=12)	4.834878
knn_v2(k=100,kd_tree,scale_ratio=120)	4.757755
knn_v2(k=100,kd_tree,scale_ratio=1200)	4.713836
knn_v2(k=100,kd_tree,scale_ratio=12000)	4.717864

decision_tree_v1(CART,Category ~ X + Y + Date2)			3.155964
decision_tree_v1(CART,Category ~ PdDistrict + Date2)		3.486093
decision_tree_v1(CART,Category ~ X + Y + Year)			3.092753
decision_tree_v1(CART,Category ~ PdDistrict)				2.637704
decision_tree_v1(CART,Category ~ X + Y)					2.955767