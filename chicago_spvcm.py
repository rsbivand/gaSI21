import numpy as np
import geopandas as gp
data = gp.read_file('chicago_MA_sf.gpkg')
upper_gp = gp.read_file('upper.gpkg')
import libpysal as ps
W2 = ps.weights.Queen.from_dataframe(upper_gp, idVariable='upper')
W2.transform = 'r'
membership = data.upper.apply(lambda x: W2.id_order.index(x)).values
y = np.log((data.med_inc_cv.values.reshape(-1,1)+1))
X = np.log((data[['med_inc_acs', 'vacancy_rate', 'old_rate',
    'black_rate', 'hisp_rate', 'group_pop', 'dens']].values+1))
import spvcm
vcse = spvcm.Upper_SE(y, X, M=W2, membership=membership, n_samples=5000, 
    configs=dict(tuning=1000, adapt_step=1.01))
trace_dataframe = vcse.trace.to_df()
import pandas as pd
trace_dataframe.to_csv("trace_dataframe.csv")
