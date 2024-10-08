# pesticide

![version](https://img.shields.io/badge/dynamic/toml?url=https%3A%2F%2Fraw.githubusercontent.com%2Ffawdlstty%2Fpesticide%2Fmain%2FCargo.toml&query=package.version&label=version)
![status](https://img.shields.io/github/actions/workflow/status/fawdlstty/pesticide/rust.yml)

基于pest库实现的编译器领域的ORM。简而言之，定义好了对象模型，即完成语言解析模块的开发。

## 文档

举个最简单的例子：

```rust
use pesticide::pesticide;

#[pesticide]
mod ast {
    pub struct Expr {
        pub val1: i32,
        #[atomic(r#" "+" | "-" | "*" | "/" "#)]
        pub op: String,
        pub val2: i32,
    }
}

fn main() {
    let expr = ast::Expr::try_parse(r#"2+3"#).unwrap();
    println!("ast: {:?}", e);
}
```

编译输出结果：

```log
ast: Expr { val1: 2, op: "+", val2: 3 }
```

文法解析器从未如此简单！下面说说代码框架：

```rust
use pesticide::pesticide;

#[pesticide]
mod ast {
    // 这里补充结构体或枚举类型的定义
}
```

支持两种类型的定义，struct和enum。结构体意味着成员为“且”关系，从文法中读取到所有成员，才能正确解析为结构体；枚举类型则表示成员为“或”关系，当某个枚举项能成功解析时，就成功解析此枚举类型。

示例：解析“2+3”

```rust
pub struct Expr {
    pub val1: i32,
    #[atomic(r#" "+" | "-" | "*" | "/" "#)]
    pub op: String,
    pub val2: i32,
}
```

示例：解析“一个整数或一个bool类型”

```rust
pub enum IntOrBool {
    Int(i32),
    Bool(bool),
}
```

默认支持的类型：`bool、u8~u64、usize、i8~i64、isize、f32、f64、String`。意味着这些类型可以不用定义解析规则，可直接使用。

结构体成员还支持两种特殊类型：Option<>、Vec<>，前者代表重复次数为0次或1次，后者可根据属性值指定重复次数。

对于成员可以指定一些特殊属性：

- #[normal]：指定当前成员的语法解析规则（pest语法）
- #[atomic]：指定当前成员的原子语法解析规则（pest语法）
    + 与normal区别是，原子语法意味着它的组成成分之间不能插入空白字符或者注释
- #[ID]: 等同于：`#[atomic(r#" (ASCII_ALPHA | "_") ~ (ASCII_ALPHANUMERIC | "_")* "#)]`
- #[silent]: 指定当前成员的语法解析规则（pest语法）
    + 与normal区别是，静音类型不会实际解析为值，通常用于指定文法中的静态内容。示例：
    ```rust
    // 枚举类型里的silent，用于指定不带值的枚举项
    pub enum NullOrIntOrBool {
        #[silent(r#" "null" "#)]
        Null,
        Int(i32),
        Bool(bool),
    }

    // 结构体里的silent，用于指定静态文本（不需要值）的属性
    pub struct Expr {
        #[silent(r#" "select" "#)]
        select: (),
        #[ID]
        #[repeat(char = '+', split = ',', last_split = false)]
        fields: Vec<String>,
        #[silent(r#" "from" "#)]
        from: (),
        #[ID]
        table_name: String,
    }
    ```
- #[ignore] 或 #[ignore(init_value=/*expr*/)]：指定当前成员不参与解析，仅用于后续自定义处理。init_value代表构造此对象的初值，如果不指定会报错，那么需指定初值

## TODO

- enum类型支持Option<>
- enum类型支持Vec<>
- 属性传递
- 可见性传递
- 内嵌表达式字符串