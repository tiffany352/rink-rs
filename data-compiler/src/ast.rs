use kdl::KdlNode;

use crate::{
    matcher::{parse_node, NodeShape},
    Error,
};

macro_rules! node_shape {
  (
      $name:ident
      $( $( $value_name:ident : $value_ty:ty )+ )?
      $( ; $( $prop_name:ident = $prop_ty:ty )+ )?
      => $node_ty:ty $block:block
  ) => {
      impl NodeShape for $node_ty {
          const NAME: &'static str = stringify!($name);
          const PROP_NAMES: &'static [&'static str] = &[ $( $(stringify!($prop_name)),* )? ];
          type Props = ( $( $($prop_ty),* ,)? );
          type Values = ( $( $($value_ty),* ,)? );

          fn parse(( $( $( $value_name ),* ,)? ): Self::Values, ( $( $( $prop_name ),* ,)? ): Self::Props) -> Self {
              $block
          }
      }
  };
}

pub struct Title {
    pub text: String,
    pub lang: String,
}

node_shape!(
  title text:String ; lang=String => Title {
      Title { text, lang }
  }
);

pub struct Doc {
    pub text: String,
    pub lang: String,
}

node_shape!(
  doc text:String ; lang=String => Doc {
      Doc { text, lang }
  }
);

pub struct Alias(pub String);

node_shape!(
  alias name:String => Alias {
      Alias(name)
  }
);

pub struct Category {
    pub name: String,
}

node_shape!(
  category name:String => Category {
      Category { name }
  }
);

pub struct Unit {
    pub name: String,
    pub definition: String,
}

node_shape!(
  unit name:String definition:String => Unit {
      Unit { name, definition }
  }
);

pub struct BaseUnit {
    pub name: String,
    pub short: Option<String>,
}

node_shape!(
  base_unit name:String ; short=Option<String> => BaseUnit {
      BaseUnit { name, short }
  }
);

pub struct Prefix {
    pub value: String,
    pub short: Option<String>,
    pub long: Option<String>,
}

node_shape!(
  prefix value:String ; short=Option<String> long=Option<String> => Prefix {
      Prefix { value, short, long }
  }
);

pub struct Quantity {
    pub name: String,
    pub dimensionality: String,
}

node_shape!(
  quantity name:String dimensionality:String => Quantity {
      Quantity { name, dimensionality }
  }
);

pub struct Substance {
    pub name: String,
    pub symbol: Option<String>,
}

node_shape!(
  substance name:String symbol:Option<String> => Substance {
      Substance { name, symbol }
  }
);

pub struct Property {
    pub name: String,
    pub input_name: String,
    pub input_value: String,
    pub output_name: String,
    pub output_value: String,
}

node_shape!(
  prop name:String input_name:String input_value:String output_name:String output_value:String => Property {
      Property { name, input_name, input_value, output_name, output_value }
  }
);

pub enum NodeData {
    Title(Title),
    Doc(Doc),
    Alias(Alias),
    Category(Category),
    Unit(Unit),
    BaseUnit(BaseUnit),
    Prefix(Prefix),
    Quantity(Quantity),
    Substance(Substance),
    Property(Property),
}

impl NodeData {
    fn parse(node: &KdlNode) -> Result<NodeData, Error> {
        match &*node.name {
            Title::NAME => parse_node(node).map(NodeData::Title),
            Doc::NAME => parse_node(node).map(NodeData::Doc),
            Alias::NAME => parse_node(node).map(NodeData::Alias),
            Category::NAME => parse_node(node).map(NodeData::Category),
            Unit::NAME => parse_node(node).map(NodeData::Unit),
            BaseUnit::NAME => parse_node(node).map(NodeData::BaseUnit),
            Prefix::NAME => parse_node(node).map(NodeData::Prefix),
            Quantity::NAME => parse_node(node).map(NodeData::Quantity),
            Substance::NAME => parse_node(node).map(NodeData::Substance),
            Property::NAME => parse_node(node).map(NodeData::Property),
            _ => return Err(Error::UnknownNodeTag { node: node.clone() }),
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            NodeData::Title(_) => Title::NAME,
            NodeData::Doc(_) => Doc::NAME,
            NodeData::Alias(_) => Alias::NAME,
            NodeData::Category(_) => Category::NAME,
            NodeData::Unit(_) => Unit::NAME,
            NodeData::BaseUnit(_) => BaseUnit::NAME,
            NodeData::Prefix(_) => Prefix::NAME,
            NodeData::Quantity(_) => Quantity::NAME,
            NodeData::Substance(_) => Substance::NAME,
            NodeData::Property(_) => Property::NAME,
        }
    }
}

pub struct Node {
    pub data: NodeData,
    pub children: Vec<Node>,
}

impl Node {
    pub fn parse(node: &KdlNode) -> Result<Node, Error> {
        let data = NodeData::parse(node)?;
        let children = node
            .children
            .iter()
            .map(Node::parse)
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(Node { data, children })
    }
}
