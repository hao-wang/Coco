Coco（Cosmos cOnstraint COde）是在多方借鉴(感谢easyLTB和CosmoMC和Numerical Recipe)的基础上完成的。旨在尽可能简单甚至自动化地完成FRW框架下（物质分布均匀各向同性）对宇宙学模型参数的限制和画contour图的任务。
Coco的特点：
可以按普通和mcmc两种模式完成画contour的任务；
结构自然，很容易读懂；
添加参数和观测数据简单；
可以自动生成画图程序（IDL文件）;
在Linux和Windows下都可以方便地运行;
每次运行的时间，运行时所采用的模型和参数设置情况，都保存在名为info.txt的文件中；

主要程序文件介绍：
Coco.f90是程序入口，在这里选择是使用普通方法(逐个格点计算开方值,use_MCMC=0)还是使用MCMC方法(use_MCMC=1)；
Settings.f90是设置宇宙学模型的地方；
ReadInData.f90读入数据文件或者（数据点很少时）接受数据点赋值；在这里选择使用哪些观测，默认使用包括SN，Hz, CMB, BAO，BAOP在内的五组数据，可以通过直接注释掉的办法弃用某组数据，而且这样做不会导致进行其他修改；
Chi2.f90负责计算chi square；
ParamScan.f90视use_MCMC的取值进行逐格点扫描参数空间，或者以随机方式在参数空间行走，得到一个由参数点构成的markov链；
GenIDL.f90根据参数和所选观测的情况生成一个IDL文件，在该文件开始设置一下目录之后，直接运行即可得到一个描述对各参数限制的ps文件；

如何使用：
0 程序自动生成IDL画图程序，如果要利用这一点，就需要安装IDL.我用的是IDL 7.1。此外需要下载两个IDL程序库，textoidl以及Coyote(http://www.ast.cam.ac.uk/~vasily/idl.htm)；将他们所在的位置添加至IDL路径。
1-Windows
1)用compaq visual fortran打开coco.dsw文件(intel visual fortran没用过，不知道怎么样)；
	编辑coco.f90文件，选择是否使用mcmc方法；
	编辑readdata.f90文件，添加数据或者注释掉不想要的数据(在ReadInObsData函数中进行，比如，不使用sn择注释掉'call read_sn')；
	编辑settings.f90文件，设置模型和参数，你可能需要花些时间读一下这个文件，它集中了几乎所有需要设置的变量；
	设置io.f90文件中的plotdir，即生成的数据和idl作图程序放在哪里，比如plotdir='D:\'，不改的话就是生成在coco文件夹；
	编译，运行；
2)用idl打开生成的.pro文件，如LCDM_mcmc.pro；
	设置plotdir为.pro文件所在位置的绝对路径，比如plotdir='D:\coco\'
	编译该文件；
	在‘命令行'中运行LCDM_mcmc, x;(x为0或者1，见.pro文件开头的注释)；
3)得到contour图(LCDMSN_mc.ps, LCDMHz_mc.ps...)。	
NOTE: since Mar 2012, the project file(coco.dsp, coco.dsw) will no longer be given. You can still run coco using CVF: adding all coco .f90 files to a blank project and compiling coco.f90, you will get coco.dsp and coco.dsw!

2-Linux
1)进入coco目录；
	编辑coco.f90文件，选择是否使用mcmc方法；	
	编辑readdata.f90文件，添加数据或者注释掉不想要的数据(在ReadInObsData函数中进行，比如，不使用sn择注释掉'call read_sn')；
	编辑settings.f90文件，设置模型和参数，你可能需要花些时间读一下这个文件，它集中了几乎所有需要设置的变量；
	设置io.f90文件中的plotdir，比如plotdir='./plot/'(需要先建立一个plot文件夹)，不改的话就是生成在coco文件夹；
	make；
2)在生成的.pro文件（如LCDM_mcmc.pro）所在目录下开启idl；
	编译文件，如；
	运行LCDM_mcmc, x;(x为0或者1，见.pro文件开头的注释)；
3)得到contour图(LCDMSN_mc.ps, LCDMHz_mc.ps...)。	
