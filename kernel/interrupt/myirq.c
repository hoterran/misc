#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/interrupt.h>

MODULE_LICENSE("GPL");

static int irq;
static char* devname;

module_param(devname, charp, 0644);
module_param(irq, int, 0644);

struct myirq {
    int devid;
};

struct myirq mydev = {1119};

static irqreturn_t myirq_handler(int irq,void* dev);

static int myirq_init(void) {
    printk(KERN_ALERT "start myirq\n");
    if (request_irq(irq, myirq_handler, IRQF_SHARED, devname, &mydev) != 0) {
        printk("%s request IRQ: %d failed .. \n", devname, irq); 
    }
    printk("%s request IRQ:%d success..\n", devname, irq);
    return 0;
}

static irqreturn_t myirq_handler(int irq,void* dev) {
    struct myirq mydev;
    static int count=1;
    mydev=*(struct myirq*)dev;
    printk("key: %d..\n",count);
    printk("devid:%d ISR is working..\n",mydev.devid);
    printk("ISR is leaving..\n");
    count++;
    return IRQ_HANDLED;
}

static void myirq_exit(void) {
    printk(KERN_ALERT "stop myirq\n");
    free_irq(irq, &mydev);
    printk("%s request IRQ:%d success..\n",devname, irq);
}

module_init(myirq_init);
module_exit(myirq_exit);

