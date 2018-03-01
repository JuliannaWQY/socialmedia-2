// var myChart = echarts.init(document.getElementById('chart1'));
var chartContianer = document.getElementById('chart1');

//用于使chart自适应高度和宽度,通过窗体高宽计算容器高宽
var resizeContainer = function() {
    chartContianer.style.width = window.innerWidth * 0.8 + 'px';
    chartContianer.style.height = window.innerHeight * 0.8 + 'px';
};
//设置容器高宽
resizeContainer();
// 基于准备好的dom，初始化echarts实例
var myChart = echarts.init(chartContianer);
var option = {
    title: {
        text: '1949-1984泰国共产党在人民日报上被提及的次数'
    },
    tooltip: {
        trigger: 'axis',
    },
    legend: {
        data: ['泰共被提及次数'],
        right: '10%'
    },
    grid: {
        left: '3%',
        right: '4%',
        bottom: '3%',
        containLabel: true
    },
    xAxis: [{
        type: 'category',
        name: '年份',
        boundaryGap: true,
        axisTick: {
            alignWithLabel: true,
            interval: 0,
        },
        data: ['1949', '1950', '1951', '1952', '1953', '1954', '1955', '1956', '1957', '1958', '1959', '1960', '1961', '1962', '1963', '1964', '1965', '1966', '1967', '1968', '1969', '1970', '1971', '1972', '1973', '1974', '1975', '1976', '1977', '1978', '1979', '1980', '1981', '1982', '1983', '1984']
    }],
    yAxis: [{
        type: 'value',
        name: '次数'
    }],
    series: [{
        name: '泰共被提及次数',
        type: 'line',
        label: {
            normal: {
                show: true,
                //position: 'top'
            }
        },
        areaStyle: { normal: {} },
        data: [2, 1, 0, 1, 0, 0, 0, 2, 2, 0, 2, 4, 7, 0, 0, 1, 1, 5, 24, 93, 93, 54, 26, 17, 2, 3, 7, 13, 7, 2, 1, 0, 1, 1, 1, 1]
    }]
};
myChart.setOption(option)

//用于使chart自适应高度和宽度
window.onresize = function() {
    //重置容器高宽
    resizeContainer();
    myChart.resize();
}