
# Table of Contents

1.  [总任务](#org4be0e91)
    1.  [audioserver <code>[6/9]</code>](#org62f0993)
        1.  [在 tang-cache 中预留一些 groupid，预留1000000](#orgb8ab413):trunk:dev:
        2.  [audioserver 多进程，直接在配置文件中添加配置项 CHILD<sub>PROCESS</sub><sub>NUMBER</sub>=子进程数量](#orgbb29120):trunk:dev:
        3.  [audioserver 添加监控会议、人数日志，重要接口操作时间的日志](#org1b57001):trunk:dev:
        4.  [白名单名字 bug 修复合并](#org1c87b11):trunk:dev:
        5.  [下周 bms 解析测试会的属性，audioserver 给 acm 通知测试会，在会议开启的时候 audioserver 将 acm 的测试会设置成 true](#orgef64394):trunk:dev:
        6.  [在 tang-cache 中预留一些 groupid，预留1000000](#orgbe78bd8):trunk:dev:
        7.  [<code>[0%]</code> 需要将 pincode 放到 userdefine 中去带到子会去绑定，修改5213信令去设置到 userdefined，在 pinchange 的时候也要更新 userdefined，MEETINGQA-24348](#orgc933ef0)
        8.  [<code>[0%]</code> bug yh8495 需要线下复现，然后删除 audiosever 中 userrolechange 和 usersrolechange](#org6f3493d)
    2.  [dsserver <code>[1/2]</code>](#org35046f6)
        1.  [桌面4K提测，增加一组信令](#org463f3c0)
        2.  [<code>[0%]</code> 桌面添加共享模式，和多共享数量限制](#org685db25)
    3.  [wbserver <code>[0/1]</code>](#orgae66d02)
        1.  [<code>[33%]</code> 白板要添加 cmduserquitgroup/cmduserquitconference 接口，PLB-7491](#org5b1a6a3)
    4.  [bms <code>[1/2]</code>](#orgcf32230)
        1.  [bms 0x182 changeConfig 给66key补刀设置为2，大方模式触发](#org52d27ea)
        2.  [<code>[33%]</code> 下周 bms 解析会议时长属性添加到 redis，tang-cache 添加获取时长接口, wiki](#orgdf33ffd)
    5.  [libacctrans <code>[0/1]</code>](#orgf4b23a5)
        1.  [<code>[0%]</code> 下周给共享声音添加到 redis，同时添加一个信令让 mixer 切换后去查询或者下发给 mixer &#x2013;召伟找我沟通的](#orge0d153d)
2.  [audioserver 优化分支](#org0be9389)
    1.  [<code>[0%]</code> 推下 audioserver 优化上线](#org770ffe4)

\#+TITLE 我的任务列表


<a id="org4be0e91"></a>

# 总任务


<a id="org62f0993"></a>

## audioserver <code>[6/9]</code>


<a id="orgb8ab413"></a>

### DONE 在 tang-cache 中预留一些 groupid，预留1000000     :trunk:dev:


<a id="orgbb29120"></a>

### DONE audioserver 多进程，直接在配置文件中添加配置项 CHILD<sub>PROCESS</sub><sub>NUMBER</sub>=子进程数量     :trunk:dev:


<a id="org1b57001"></a>

### DONE audioserver 添加监控会议、人数日志，重要接口操作时间的日志     :trunk:dev:


<a id="org1c87b11"></a>

### DONE 白名单名字 bug 修复合并     :trunk:dev:


<a id="orgef64394"></a>

### DONE 下周 bms 解析测试会的属性，audioserver 给 acm 通知测试会，在会议开启的时候 audioserver 将 acm 的测试会设置成 true     :trunk:dev:


<a id="orgbe78bd8"></a>

### DONE 在 tang-cache 中预留一些 groupid，预留1000000     :trunk:dev:


<a id="orgc933ef0"></a>

### TODO <code>[0%]</code> 需要将 pincode 放到 userdefine 中去带到子会去绑定，修改5213信令去设置到 userdefined，在 pinchange 的时候也要更新 userdefined，[MEETINGQA-24348](https://jira.quanshi.com/browse/MEETINGQA-24348)

-   [ ] trunk
-   [ ] dev


<a id="org6f3493d"></a>

### TODO <code>[0%]</code> bug [yh8495](https://jira.quanshi.com/browse/YHYKHBUG-8495) 需要线下复现，然后删除 audiosever 中 userrolechange 和 usersrolechange

-   [ ] trunk
-   [ ] dev


<a id="org35046f6"></a>

## dsserver <code>[1/2]</code>


<a id="org463f3c0"></a>

### DONE 桌面4K提测，增加一组信令


<a id="org685db25"></a>

### DOING <code>[0%]</code> 桌面添加共享模式，和多共享数量限制

1.  TODO 共享的优先级（第一个还是最后一个）这期 <span class="timestamp-wrapper"><span class="timestamp">&lt;2022-08-03 Wed&gt; </span></span> 不急实现，下来排期实现就行 [wiki](https://wiki.quanshi.com/pages/viewpage.action?pageId=70618111)

2.  [ ] trunk

3.  [X] dev


<a id="orgae66d02"></a>

## wbserver <code>[0/1]</code>


<a id="org5b1a6a3"></a>

### TODO <code>[33%]</code> 白板要添加 cmduserquitgroup/cmduserquitconference 接口，[PLB-7491](https://jira.quanshi.com/browse/PLB-7491)

-   [ ] trunk
-   [X] dev 已经提交出包


<a id="orgcf32230"></a>

## bms <code>[1/2]</code>


<a id="org52d27ea"></a>

### DONE bms 0x182 changeConfig 给66key补刀设置为2，大方模式触发


<a id="orgdf33ffd"></a>

### TODO <code>[33%]</code> 下周 bms 解析会议时长属性添加到 redis，tang-cache 添加获取时长接口, [wiki](https://wiki.quanshi.com/pages/viewpage.action?pageId=66677328)

-   [ ] trunk
-   [X] dev 已经提交出包


<a id="orgf4b23a5"></a>

## libacctrans <code>[0/1]</code>


<a id="orge0d153d"></a>

### TODO <code>[0%]</code> 下周给共享声音添加到 redis，同时添加一个信令让 mixer 切换后去查询或者下发给 mixer &#x2013;召伟找我沟通的

-   [ ] trunk
-   [ ] dev


<a id="org0be9389"></a>

# audioserver 优化分支


<a id="org770ffe4"></a>

## TODO <code>[0%]</code> 推下 audioserver 优化上线

-   [ ] trunk
-   [ ] dev

