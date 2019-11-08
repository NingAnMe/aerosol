import os
import pickle
from spectral.io import envi

# 需要使用标准的LF换行符
# data数据集的lines数量必须是met数据集的lines数据的10倍

root_path = os.path.dirname(os.path.dirname(os.path.abspath(__name__)))

in_dir = os.path.join(root_path, r'test_data/aerosol_test_input')
print(in_dir)
out_dir = r'C:\E\Projects\IMAPP\IMAPP_MODISL2_INPUT_OUTPUT\output\2011_302_Terra_1935_013223\new'

assert os.path.isdir(in_dir) is True

# if not os.path.isdir(out_dir):
#     os.makedirs(out_dir)

filenames = ['a1.17299.1910.1000m.hdr',
             'a1.17299.1910.geo.hdr',
             'a1.17299.1910.mod35.hdr',
             'a1.17299.1910.mod35qa.hdr',
             'a1.17299.1910.met.hdr'
             ]

metadatas = {}

for filename in filenames:
    in_file = os.path.join(in_dir, filename)
    out_file = os.path.join(out_dir, filename)
    print('hdr file: {}'.format(in_file))

    metadata = envi.read_envi_header(in_file)
    interleave = metadata.pop('interleave')
    metadata.pop('description')
    file_type = filename.split('.')[-2]
    metadatas[file_type] = {'metadata': metadata, 'interleave': interleave}

    data = envi.open(in_file).asarray()
    import numpy as np
    import matplotlib.pyplot as plt
    data = np.array(data)
    print(data.shape)
    # for i in range(36):
    #     _data = data[:, :, i]
    #     print(type(_data))
    #     _data[_data == -1] = np.nan
    #     print('i = {} min = {} max = {}'.format(i, np.nanmin(_data), np.nanmax(_data)))
    #     # plt.hist(_data, 100)
    #     # plt.savefig('{}_{}.jpg'.format(filename, i))
    # data_new = data
    # print(data_new.shape)
    # envi.save_image(out_file, data_new, metadata=metadata, force=True, interleave=interleave)

with open('metadatas.pickle', 'wb') as fp:
    pickle.dump(metadatas, fp)

# filename = 't1.11302.1935.met.hdr'
# in_file = os.path.join(in_dir, filename)
# metadata = envi.read_envi_header(in_file)
# interleave = metadata['interleave']
# data = envi.open(in_file).asarray()
# data_new = data[:150]
# print(data_new.shape)
# envi.save_image(os.path.join(out_dir, filename), data_new, metadata=metadata, force=True, interleave=interleave)
