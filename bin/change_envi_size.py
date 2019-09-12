import os
import pickle
from spectral.io import envi

# 需要使用标准的LF换行符
# data数据集的lines数量必须是met数据集的lines数据的10倍

in_dir = r'C:\E\Projects\IMAPP\IMAPP_MODISL2_INPUT_OUTPUT\output\2011_302_Terra_1935_013223'
out_dir = r'C:\E\Projects\IMAPP\IMAPP_MODISL2_INPUT_OUTPUT\output\2011_302_Terra_1935_013223\new'

if not os.path.isdir(out_dir):
    os.makedirs(out_dir)

filenames = ['t1.11302.1935.1000m.hdr',
             't1.11302.1935.geo.hdr',
             't1.11302.1935.mod35.hdr',
             't1.11302.1935.mod35qa.hdr',
             't1.11302.1935.met.hdr'
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
    data_new = data
    print(data_new.shape)
    envi.save_image(out_file, data_new, metadata=metadata, force=True, interleave=interleave)

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
